{-# LANGUAGE RecordWildCards #-}
-- | Mid-level operations on ChatData
-- All operations use FilePage abstractions, no direct file I/O
module Web.Convent.Storage.ChatDataOps
  ( ChatData(..)
  , getCurrentTimestamp
  , getNextOffsetFromChatData
  , addEventToChatData
  , getEventsFromChatData
  , extractAllEventsFromChatData
  , EventResponse(..)
  ) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, except, throwE)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Posix.Types (Fd)
import Web.Convent.Storage.IndexPage (IndexPage)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import qualified Web.Convent.Storage.FilePage as FilePage

-- | Represents data associated with a single chat
data ChatData = ChatData
  { chatDataIndexFd :: Fd                   -- ^ File descriptor for the index file
  , chatDataEventsFd :: Fd                  -- ^ File descriptor for the events file
  , chatDataCachedIndexPages :: Map.Map Int IndexPage  -- ^ Currently loaded index pages
  , chatDataCachedEventPages :: Map.Map Int EventsPage -- ^ Currently loaded event pages
  , chatDataIndexPageCount :: Int            -- ^ Number of index pages
  , chatDataEventsPageCount :: Int           -- ^ Number of events pages
  }

-- | Event response data for API
data EventResponse = EventResponse
  { responseEventOffset :: Word64
  , responseEventType :: Word64
  , responseEventData :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON EventResponse
instance FromJSON EventResponse

-- | ChatDataState monad: ExceptT for errors, StateT for ChatData updates
type ChatDataState a = ExceptT String (StateT ChatData IO) a

-- | Run a ChatDataState computation and return result with updated ChatData
runChatDataState :: ChatDataState a -> ChatData -> IO (Either String a, ChatData)
runChatDataState action chatData = runStateT (runExceptT action) chatData

-- | Load an index page (check cache first, load from file if needed, update cache)
loadIndexPage :: Int -> ChatDataState IndexPage
loadIndexPage pageIdx = do
  chatData <- lift get
  case Map.lookup pageIdx (chatDataCachedIndexPages chatData) of
    Just page -> return page  -- Cache hit
    Nothing -> do
      -- Cache miss - load from file
      eitherPage <- lift $ lift $ FilePage.load (chatDataIndexFd chatData) (FilePage.Index pageIdx, FilePage.Size 8192)
      case eitherPage of
        Left _ -> throwE "Failed to load index page"
        Right page -> do
          -- Update cache
          let newCachedPages = Map.insert pageIdx page (chatDataCachedIndexPages chatData)
          lift $ put $ chatData { chatDataCachedIndexPages = newCachedPages }
          return page

-- | Load an events page (check cache first, load from file if needed, update cache)
loadEventsPage :: Int -> ChatDataState EventsPage
loadEventsPage pageIdx = do
  chatData <- lift get
  case Map.lookup pageIdx (chatDataCachedEventPages chatData) of
    Just page -> return page  -- Cache hit
    Nothing -> do
      -- Cache miss - load from file
      eitherPage <- lift $ lift $ FilePage.load (chatDataEventsFd chatData) (FilePage.Index pageIdx, FilePage.Size 8192)
      case eitherPage of
        Left _ -> throwE "Failed to load events page"
        Right page -> do
          -- Update cache
          let newCachedPages = Map.insert pageIdx page (chatDataCachedEventPages chatData)
          lift $ put $ chatData { chatDataCachedEventPages = newCachedPages }
          return page

-- | Save an index page (write to file and update cache atomically)
saveIndexPage :: Int -> IndexPage -> ChatDataState ()
saveIndexPage pageIdx page = do
  chatData <- lift get
  -- Write to file
  eitherUnit <- lift $ lift $ FilePage.save (chatDataIndexFd chatData) (FilePage.Index pageIdx, FilePage.Size 8192) page
  case eitherUnit of
    Left _ -> throwE "Failed to save index page"
    Right () -> do
      -- Update cache
      let newCachedPages = Map.insert pageIdx page (chatDataCachedIndexPages chatData)
      lift $ put $ chatData { chatDataCachedIndexPages = newCachedPages }

-- | Save an events page (write to file and update cache atomically)
saveEventsPage :: Int -> EventsPage -> ChatDataState ()
saveEventsPage pageIdx page = do
  chatData <- lift get
  -- Write to file
  eitherUnit <- lift $ lift $ FilePage.save (chatDataEventsFd chatData) (FilePage.Index pageIdx, FilePage.Size 8192) page
  case eitherUnit of
    Left _ -> throwE "Failed to save events page"
    Right () -> do
      -- Update cache
      let newCachedPages = Map.insert pageIdx page (chatDataCachedEventPages chatData)
      lift $ put $ chatData { chatDataCachedEventPages = newCachedPages }

-- | Get current timestamp with microsecond precision to avoid collisions
getCurrentTimestamp :: IO Word64
getCurrentTimestamp = do
  posixTime <- getPOSIXTime
  -- Convert to microseconds to avoid collisions within same second
  return $ round (posixTime * 1000000)

-- | Calculate next event offset from ChatData (reads from cached pages and index)
getNextOffsetFromChatData :: ChatData -> IO (Either String Word64)
getNextOffsetFromChatData chatData = 
  -- If no events pages exist, offset is 0
  if chatDataEventsPageCount chatData == 0
    then return $ Right 0
    else do
      (result, _updatedChatData) <- runChatDataState getNextOffsetState chatData
      return result

-- | Internal implementation using ChatDataState monad
getNextOffsetState :: ChatDataState Word64
getNextOffsetState = do
  chatData <- lift get
  
  -- Get the last index page
  let lastIndexPageIdx = chatDataIndexPageCount chatData - 1
  lastIndexPage <- loadIndexPage lastIndexPageIdx
  
  let indexEntries = IndexPage.entries lastIndexPage
  
  if null indexEntries
    then do
      -- No index entries yet, get count from first events page (page 0)
      firstEventsPage <- loadEventsPage 0
      return $ fromIntegral $ EventsPage.eventCount firstEventsPage
    else do
      -- Get last entry to find base offset and corresponding page
      let lastEntry = last indexEntries
          -- Index entries correspond to pages 1, 2, 3, etc. (page 0 has no entry)
          lastPageIdx = length indexEntries
          baseOffset = IndexPage.minimumEventOffset lastEntry
      
      -- Get the corresponding events page
      lastEventsPage <- loadEventsPage lastPageIdx
      
      let eventsInPage = fromIntegral $ EventsPage.eventCount lastEventsPage
      return $ baseOffset + eventsInPage

-- | Add an event to ChatData (mid-level abstraction)
-- Returns the new ChatData and the event offset
addEventToChatData :: ChatData -> BS.ByteString -> IO (Either String (ChatData, Word64))
addEventToChatData chatData eventData = do
  (result, newChatData) <- runChatDataState (addEventToChatDataState eventData) chatData
  case result of
    Left err -> return $ Left err
    Right offset -> return $ Right (newChatData, offset)

-- | Internal implementation using ChatDataState monad
addEventToChatDataState :: BS.ByteString -> ChatDataState Word64
addEventToChatDataState eventData = do
  chatData <- lift get
  
  -- Calculate the offset for this event
  eitherOffset <- lift $ lift $ getNextOffsetFromChatData chatData
  eventOffset <- except eitherOffset
  
  -- Handle first page case
  when (chatDataEventsPageCount chatData == 0) $ throwE "No events pages (should have been initialized)"
  
  -- Get the last events page
  let lastEventsPageIdx = chatDataEventsPageCount chatData - 1
  lastEventsPage <- loadEventsPage lastEventsPageIdx
  
  -- Try to add event to last page
  case EventsPage.addEvent lastEventsPage eventData of
    Just newPage -> do
      -- Event fits in current page, save it
      saveEventsPage lastEventsPageIdx newPage
      return eventOffset
    Nothing -> do
      -- Page is full, need to create new page and update index
      let newEventsPage = EventsPage.emptyPage
      case EventsPage.addEvent newEventsPage eventData of
        Nothing -> throwE "Event too large for empty page"
        Just finalPage -> do
          -- Append new events page
          let newEventsPageIdx = chatDataEventsPageCount chatData
          saveEventsPage newEventsPageIdx finalPage
          
          -- Update index with new entry
          let lastIndexPageIdx = chatDataIndexPageCount chatData - 1
          lastIndexPage <- loadIndexPage lastIndexPageIdx
          
          case IndexPage.addEntry lastIndexPage eventOffset of
            Left _ -> throwE "Index full or invalid offset"
            Right newIndexPage -> do
              -- Store updated index page
              saveIndexPage lastIndexPageIdx newIndexPage
              
              -- Update page count
              chatData' <- lift get
              lift $ put $ chatData' { chatDataEventsPageCount = newEventsPageIdx + 1 }
              
              return eventOffset

-- | Get events from ChatData starting from an offset
getEventsFromChatData :: ChatData -> Word64 -> IO (Either String [EventResponse])
getEventsFromChatData ChatData{..} startOffset = runExceptT $ do
  -- Load all events pages and extract events
  allEvents <- lift $ extractAllEventsFromChatData ChatData{..} 0 chatDataEventsPageCount 0
  
  -- Safe conversion of Word64 offset to Int for drop
  let maxDrop :: Int
      maxDrop = maxBound
      safeDrop :: Int
      safeDrop =
        if startOffset > fromIntegral maxDrop
          then maxDrop
          else fromIntegral startOffset
  
  return $ drop safeDrop allEvents

-- | Extract all events from ChatData (helper for getEventsFromChatData)
extractAllEventsFromChatData :: ChatData -> Int -> Int -> Word64 -> IO [EventResponse]
extractAllEventsFromChatData chatData pageIdx numPages cumulativeOffset
  | pageIdx >= numPages = return []
  | otherwise = do
      -- Use ChatDataState to benefit from cache management
      (result, _updatedChatData) <- runChatDataState (extractEventsFromPage pageIdx) chatData
      case result of
        Left _ -> extractAllEventsFromChatData chatData (pageIdx + 1) numPages cumulativeOffset
        Right (eventsInPage, nextOffset) -> do
          rest <- extractAllEventsFromChatData chatData (pageIdx + 1) numPages nextOffset
          return $ eventsInPage ++ rest
  where
    extractEventsFromPage :: Int -> ChatDataState ([EventResponse], Word64)
    extractEventsFromPage idx = do
      page <- loadEventsPage idx
      let numEventsWord = EventsPage.eventCount page
          numEvents :: Int
          numEvents = fromIntegral numEventsWord
          eventsInPage =
            if numEventsWord == 0
              then []
              else map (extractEvent page cumulativeOffset) [0 .. numEvents - 1]
          nextOffset = cumulativeOffset + fromIntegral numEventsWord
      return (eventsInPage, nextOffset)
    
    extractEvent page baseOffset idx = 
      let maybeEventBS = EventsPage.event page (fromIntegral idx)
          (eventTypeId, eventDataBS) = case maybeEventBS of
            Nothing -> (0, BS.empty)
            Just eventBS -> 
              if BS.length eventBS > 0 
              then (BS.index eventBS 0, eventBS)
              else (0, BS.empty)
          globalOffset = baseOffset + fromIntegral idx
      in EventResponse
           { responseEventOffset = globalOffset
           , responseEventType = fromIntegral eventTypeId
           , responseEventData = Text.pack $ show eventDataBS
           }
