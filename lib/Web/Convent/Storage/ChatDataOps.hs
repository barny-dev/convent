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
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import System.IO (Handle)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.Convent.Storage.IndexPage (IndexPage)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import qualified Web.Convent.Storage.FilePage as FilePage

-- | Represents data associated with a single chat
data ChatData = ChatData
  { chatDataIndexHandle :: Handle            -- ^ Handle to the index file
  , chatDataEventsHandle :: Handle          -- ^ Handle to the events file
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

-- | Get current timestamp with microsecond precision to avoid collisions
getCurrentTimestamp :: IO Word64
getCurrentTimestamp = do
  posixTime <- getPOSIXTime
  -- Convert to microseconds to avoid collisions within same second
  return $ round (posixTime * 1000000)

-- | Calculate next event offset from ChatData (reads from cached pages and index)
getNextOffsetFromChatData :: ChatData -> IO (Either String Word64)
getNextOffsetFromChatData ChatData{..} = runExceptT $ do
  -- If no events pages exist, offset is 0
  when (chatDataEventsPageCount == 0) $ throwE "No events pages"
  
  -- Get the last index page
  let lastIndexPageIdx = chatDataIndexPageCount - 1
  lastIndexPage <- case Map.lookup lastIndexPageIdx chatDataCachedIndexPages of
    Just page -> return page
    Nothing -> do
      -- Load from file
      eitherPage <- lift $ FilePage.load chatDataIndexHandle (FilePage.Index lastIndexPageIdx, FilePage.Size 8192)
      case eitherPage of
        Left _ -> throwE "Failed to load index page"
        Right page -> return page
  
  let indexEntries = IndexPage.entries lastIndexPage
  
  if null indexEntries
    then do
      -- No index entries yet, get count from first events page (page 0)
      firstEventsPage <- case Map.lookup 0 chatDataCachedEventPages of
        Just page -> return page
        Nothing -> do
          eitherPage <- lift $ FilePage.load chatDataEventsHandle (FilePage.Index 0, FilePage.Size 8192)
          case eitherPage of
            Left _ -> throwE "Failed to load first events page"
            Right page -> return page
      return $ fromIntegral $ EventsPage.eventCount firstEventsPage
    else do
      -- Get last entry to find base offset and corresponding page
      let lastEntry = last indexEntries
          -- Index entries correspond to pages 1, 2, 3, etc. (page 0 has no entry)
          lastPageIdx = length indexEntries
          baseOffset = IndexPage.minimumEventOffset lastEntry
      
      -- Get the corresponding events page
      lastEventsPage <- case Map.lookup lastPageIdx chatDataCachedEventPages of
        Just page -> return page
        Nothing -> do
          eitherPage <- lift $ FilePage.load chatDataEventsHandle (FilePage.Index lastPageIdx, FilePage.Size 8192)
          case eitherPage of
            Left _ -> throwE "Failed to load events page"
            Right page -> return page
      
      let eventsInPage = fromIntegral $ EventsPage.eventCount lastEventsPage
      return $ baseOffset + eventsInPage

-- | Add an event to ChatData (mid-level abstraction)
-- Returns the new ChatData and the event offset
addEventToChatData :: ChatData -> BS.ByteString -> IO (Either String (ChatData, Word64))
addEventToChatData chatData@ChatData{..} eventData = runExceptT $ do
  -- Calculate the offset for this event
  eitherOffset <- lift $ getNextOffsetFromChatData chatData
  eventOffset <- except eitherOffset
  
  -- Handle first page case
  when (chatDataEventsPageCount == 0) $ throwE "No events pages (should have been initialized)"
  
  -- Get the last events page
  let lastEventsPageIdx = chatDataEventsPageCount - 1
  lastEventsPage <- case Map.lookup lastEventsPageIdx chatDataCachedEventPages of
    Just page -> return page
    Nothing -> do
      eitherPage <- lift $ FilePage.load chatDataEventsHandle (FilePage.Index lastEventsPageIdx, FilePage.Size 8192)
      case eitherPage of
        Left _ -> throwE "Failed to load last events page"
        Right page -> return page
  
  -- Try to add event to last page
  case EventsPage.addEvent lastEventsPage eventData of
    Just newPage -> do
      -- Event fits in current page, update it
      eitherUnit <- lift $ FilePage.save chatDataEventsHandle (FilePage.Index lastEventsPageIdx, FilePage.Size 8192) newPage
      case eitherUnit of
        Left _ -> throwE "Failed to store updated events page"
        Right () -> do
          -- Update cached page
          let newCachedEventPages = Map.insert lastEventsPageIdx newPage chatDataCachedEventPages
              newChatData = chatData { chatDataCachedEventPages = newCachedEventPages }
          return (newChatData, eventOffset)
    Nothing -> do
      -- Page is full, need to create new page and update index
      let newEventsPage = EventsPage.emptyPage
      case EventsPage.addEvent newEventsPage eventData of
        Nothing -> throwE "Event too large for empty page"
        Just finalPage -> do
          -- Append new events page
          let newEventsPageIdx = chatDataEventsPageCount
          eitherUnit <- lift $ FilePage.save chatDataEventsHandle (FilePage.Index newEventsPageIdx, FilePage.Size 8192) finalPage
          case eitherUnit of
            Left _ -> throwE "Failed to store new events page"
            Right () -> do
              -- Update index with new entry
              let lastIndexPageIdx = chatDataIndexPageCount - 1
              lastIndexPage <- case Map.lookup lastIndexPageIdx chatDataCachedIndexPages of
                Just page -> return page
                Nothing -> do
                  eitherPage <- lift $ FilePage.load chatDataIndexHandle (FilePage.Index lastIndexPageIdx, FilePage.Size 8192)
                  case eitherPage of
                    Left _ -> throwE "Failed to load index page"
                    Right page -> return page
              
              case IndexPage.addEntry lastIndexPage eventOffset of
                Left _ -> throwE "Index full or invalid offset"
                Right newIndexPage -> do
                  -- Store updated index page
                  eitherUnit2 <- lift $ FilePage.save chatDataIndexHandle (FilePage.Index lastIndexPageIdx, FilePage.Size 8192) newIndexPage
                  case eitherUnit2 of
                    Left _ -> throwE "Failed to store updated index page"
                    Right () -> do
                      -- Update cached data
                      let newCachedIndexPages = Map.insert lastIndexPageIdx newIndexPage chatDataCachedIndexPages
                          newCachedEventPages = Map.insert newEventsPageIdx finalPage chatDataCachedEventPages
                          newChatData = chatData 
                            { chatDataCachedIndexPages = newCachedIndexPages
                            , chatDataCachedEventPages = newCachedEventPages
                            , chatDataEventsPageCount = newEventsPageIdx + 1
                            }
                      return (newChatData, eventOffset)

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

-- | Extract all events from ChatData
extractAllEventsFromChatData :: ChatData -> Int -> Int -> Word64 -> IO [EventResponse]
extractAllEventsFromChatData chatData pageIdx numPages cumulativeOffset
  | pageIdx >= numPages = return []
  | otherwise = do
      -- Get or load the events page
      let ChatData{..} = chatData
      eitherPage <- case Map.lookup pageIdx chatDataCachedEventPages of
        Just page -> return $ Right page
        Nothing -> FilePage.load chatDataEventsHandle (FilePage.Index pageIdx, FilePage.Size 8192)
      
      case eitherPage of
        Left _ -> extractAllEventsFromChatData chatData (pageIdx + 1) numPages cumulativeOffset
        Right page -> do
          let numEventsWord = EventsPage.eventCount page
              numEvents :: Int
              numEvents = fromIntegral numEventsWord
              eventsInPage =
                if numEventsWord == 0
                  then []
                  else map (extractEvent page cumulativeOffset) [0 .. numEvents - 1]
              nextOffset = cumulativeOffset + fromIntegral numEventsWord
          rest <- extractAllEventsFromChatData chatData (pageIdx + 1) numPages nextOffset
          return $ eventsInPage ++ rest
  where
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
