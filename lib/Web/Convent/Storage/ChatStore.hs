
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Web.Convent.Storage.ChatStore
  ( ChatStore
  , ChatStoreConfig(..)
  , ChatData(..)
  , newChatStore
  , createChat
  , withChatLock
  , getHighestEventOffset
  , chatExists
  , joinChatParticipant
  , postChatMessage
  , getChatEvents
  , EventResponse(..)
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT (..), except, withExceptT, throwE, catchE, runExceptT)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import System.IO (Handle, hFileSize)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.Convent.Storage.IndexPage (IndexPage)
import Data.Maybe (maybe)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import Web.Convent.Storage.EventStore
import qualified Web.Convent.Storage.FilePage as FilePage
import qualified Web.Convent.Events.ParticipantJoinedEvent as ParticipantJoinedEvent
import qualified Web.Convent.Events.MessageSubmittedEvent as MessageSubmittedEvent

-- | Represents data associated with a single chat
data ChatData = ChatData
  { indexHandle :: Handle            -- ^ Handle to the index file
  , eventsHandle :: Handle          -- ^ Handle to the events file
  , cachedIndexPages :: Map.Map Int IndexPage  -- ^ Currently loaded index pages
  , cachedEventPages :: Map.Map Int EventsPage -- ^ Currently loaded event pages
  , indexPageCount :: Int            -- ^ Number of index pages
  , eventsPageCount :: Int           -- ^ Number of events pages
  }

-- | Configuration for ChatStore
data ChatStoreConfig = ChatStoreConfig
  { chatsDirectory :: FilePath  -- ^ Base directory for storing chat data
  } deriving (Show, Eq)

-- | Event response data for API
data EventResponse = EventResponse
  { responseEventOffset :: Word64
  , responseEventType :: Word64
  , responseEventData :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON EventResponse
instance FromJSON EventResponse

data ChatStoreError = EventsFileCorrupted | IndexFileCorrupted deriving (Show, Eq)
-- | Given a index file handle and events file handle, creates new ChatData, making sure to load the last page of each file. Page size is 8192 bytes in both cases, so the page count can be calculated by dividing the file size by 8192.
initChatData :: Handle -> Handle -> IO (Either ChatStoreError ChatData)
initChatData indexHandle eventsHandle = runExceptT $ do
  -- Get file sizes for index and events files
    indexSize <- lift $ hFileSize indexHandle
    eventsSize <- lift $ hFileSize eventsHandle
  -- Calculate page counts
    let indexPageCount = fromIntegral (indexSize `div` 8192)
        eventsPageCount = fromIntegral (eventsSize `div` 8192)  
  -- Read last index page if available
    maybeLastIndexPage :: Maybe IndexPage <- if indexPageCount > 0
                          then withExceptT (const IndexFileCorrupted) $
                               fmap Just . ExceptT $
                               FilePage.load indexHandle (FilePage.Index (indexPageCount - 1), FilePage.Size 8192)
                          else return Nothing
  -- Read last events page if available
    maybeLastEventsPage :: Maybe EventsPage <- if eventsPageCount > 0
                           then withExceptT (const EventsFileCorrupted) $
                                fmap Just . ExceptT $
                                FilePage.load eventsHandle (FilePage.Index (eventsPageCount - 1), FilePage.Size 8192)
                           else return Nothing
  -- Cache the pages
    let cachedIndexPages = case maybeLastIndexPage of
                             Just page -> Map.singleton (indexPageCount - 1) page
                             Nothing -> Map.empty
        cachedEventPages = case maybeLastEventsPage of
                             Just page -> Map.singleton (eventsPageCount - 1) page
                             Nothing -> Map.empty
  -- Create and return ChatData
    return ChatData { indexHandle = indexHandle
                  , eventsHandle = eventsHandle
                  , cachedIndexPages = cachedIndexPages
                  , cachedEventPages = cachedEventPages
                  , indexPageCount = indexPageCount
                  , eventsPageCount = eventsPageCount
                  }

-- | Thread-safe store of chat data with configuration
data ChatStore = ChatStore 
  { chatStoreLocks :: MVar (Map.Map UUID (MVar ()))
  , chatStoreConfig :: ChatStoreConfig
  }

-- | Creates a new empty chat store with configuration
newChatStore :: ChatStoreConfig -> IO ChatStore
newChatStore config = do
  locks <- newMVar Map.empty
  return $ ChatStore locks config

-- | Creates a new chat with a unique UUID and initializes storage files
createChat :: ChatStore -> IO UUID
createChat (ChatStore _ config) = do
  -- Generate a new UUID for the chat
  uuid <- UUID.V4.nextRandom
  
  -- Create chat directory using configured base path
  let chatDir = chatsDirectory config ++ "/" ++ show uuid
  createDirectoryIfMissing True chatDir
  
  -- Create empty index and events files
  let indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  -- Create files with initial empty page
  BS.writeFile indexPath (IndexPage.toByteString IndexPage.emptyPage)
  BS.writeFile eventsPath (EventsPage.toByteString EventsPage.emptyPage)
  
  return uuid

-- | Check if a chat exists
chatExists :: ChatStore -> UUID -> IO Bool
chatExists (ChatStore _ config) uuid = do
  let chatDir = chatsDirectory config ++ "/" ++ show uuid
  doesDirectoryExist chatDir

-- | Executes an action with exclusive access to a chat (ensures serialized writes)
withChatLock :: ChatStore -> UUID -> IO a -> IO a
withChatLock (ChatStore mvar _) chatId action = do
  -- Get or create the lock for this chat
  chatLock <- modifyMVar mvar $ \store ->
    case Map.lookup chatId store of
      Just lock -> return (store, lock)
      Nothing -> do
        lock <- newMVar ()  -- Simple unit lock for serialization
        return (Map.insert chatId lock store, lock)
  
  -- Execute action with the lock held
  bracket
    (takeMVar chatLock)
    (putMVar chatLock)
    (\_ -> action)

-- | Returns the highest event offset in a chat
getHighestEventOffset :: ChatStore -> UUID -> IO (Either String Word64)
getHighestEventOffset (ChatStore _ config) uuid = do
  let chatDir = chatsDirectory config ++ "/" ++ show uuid
      indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  -- Use calculateNextOffset and subtract 1 (since it returns the *next* offset)
  nextOffset <- calculateNextOffset indexPath eventsPath
  if nextOffset == 0
    then return $ Right 0
    else return $ Right (nextOffset - 1)

-- High-level operations for Web API

-- | Join a chat as a participant
joinChatParticipant :: ChatStore -> UUID -> Text -> IO (Either String (Word64, Word64))
joinChatParticipant store@(ChatStore _ config) uuid participantName = do
  let chatDir = chatsDirectory config ++ "/" ++ show uuid
      indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  withChatLock store uuid $ do
    -- Generate participant ID with microsecond precision to avoid collisions
    timestamp <- getCurrentTimestamp
    let pid = timestamp
    
    -- Calculate next event offset using index
    eventOffset <- calculateNextOffset indexPath eventsPath
    
    -- Create and append ParticipantJoinedEvent
    let event = ParticipantJoinedEvent.Event
          { ParticipantJoinedEvent.participantId = pid
          , ParticipantJoinedEvent.timestamp = timestamp
          , ParticipantJoinedEvent.participantName = participantName
          }
    
    result <- appendEventToFile indexPath eventsPath (ParticipantJoinedEvent.encode event)
    case result of
      Left err -> return $ Left $ "Failed to append join event: " ++ err
      Right () -> return $ Right (pid, eventOffset)

-- | Post a message to a chat
postChatMessage :: ChatStore -> UUID -> Word64 -> Text -> IO (Either String Word64)
postChatMessage store@(ChatStore _ config) uuid participantId messageText = do
  let chatDir = chatsDirectory config ++ "/" ++ show uuid
      indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  withChatLock store uuid $ do
    -- Get current timestamp
    timestamp <- getCurrentTimestamp
    
    -- Calculate next event offset using index
    eventOffset <- calculateNextOffset indexPath eventsPath
    
    -- Create and append MessageSubmittedEvent
    let event = MessageSubmittedEvent.Event
          { MessageSubmittedEvent.participantId = participantId
          , MessageSubmittedEvent.timestamp = timestamp
          , MessageSubmittedEvent.message = messageText
          }
    
    result <- appendEventToFile indexPath eventsPath (MessageSubmittedEvent.encode event)
    case result of
      Left err -> return $ Left $ "Failed to append message event: " ++ err
      Right () -> return $ Right eventOffset

-- | Get events from a chat starting from an offset
getChatEvents :: ChatStore -> UUID -> Word64 -> IO (Either String [EventResponse])
getChatEvents (ChatStore _ config) uuid startOffset = do
  let chatDir = chatsDirectory config ++ "/" ++ show uuid
      eventsPath = chatDir ++ "/events.dat"
  
  -- Read events from file
  Right <$> readEventsFromFile eventsPath startOffset

-- Helper functions (moved from Web.Convent.API)

-- | Get current timestamp with microsecond precision to avoid collisions
getCurrentTimestamp :: IO Word64
getCurrentTimestamp = do
  posixTime <- getPOSIXTime
  -- Convert to microseconds to avoid collisions within same second
  return $ round (posixTime * 1000000)

-- | Calculate the next event offset by reading the index and last events page
calculateNextOffset :: FilePath -> FilePath -> IO Word64
calculateNextOffset indexPath eventsPath = do
  indexContent <- BS.readFile indexPath
  eventsContent <- BS.readFile eventsPath
  
  let pageSize = 8192
      eventsFileSize = BS.length eventsContent
      
  -- Validate file size is multiple of page size
  if eventsFileSize `mod` pageSize /= 0
    then error $ "Corrupted events file: size " ++ show eventsFileSize ++ " is not a multiple of " ++ show pageSize
    else do
      let numEventsPages = eventsFileSize `div` pageSize
      
      -- Parse the index page
      case IndexPage.fromByteString indexContent of
        Left _ -> return 0 -- Empty or corrupted index
        Right indexPage -> do
          let indexEntries = IndexPage.entries indexPage
          
          if null indexEntries
            then do
              -- No index entries yet, count events in first page (page 0)
              if numEventsPages == 0
                then return 0
                else do
                  let firstPageData = BS.take pageSize eventsContent
                  case EventsPage.fromByteString firstPageData of
                    Left _ -> return 0
                    Right page -> return $ fromIntegral $ EventsPage.eventCount page
            else do
              -- Get the last index entry
              -- Index entries correspond to pages 1, 2, 3, etc. (page 0 has no entry)
              -- If we have N entries, the last tracked page is page N (0-indexed)
              let lastEntry = last indexEntries
                  lastPageIdx = length indexEntries
                  baseOffset = IndexPage.minimumEventOffset lastEntry
              
              -- Read the corresponding events page and count its events
              if lastPageIdx >= numEventsPages
                then return baseOffset -- Page hasn't been created yet
                else do
                  let pageData = BS.take pageSize $ BS.drop (lastPageIdx * pageSize) eventsContent
                  case EventsPage.fromByteString pageData of
                    Left _ -> return baseOffset
                    Right page -> do
                      let eventsInPage = fromIntegral $ EventsPage.eventCount page
                      return $ baseOffset + eventsInPage

-- | Append an event to the events file and update index if needed
appendEventToFile :: FilePath -> FilePath -> BS.ByteString -> IO (Either String ())
appendEventToFile indexPath eventsPath eventData = do
  -- Read entire files strictly
  eventsContent <- BS.readFile eventsPath
  indexContent <- BS.readFile indexPath
  
  let pageSize = 8192
      eventsFileSize = BS.length eventsContent
      
  -- Validate file size is multiple of page size
  if eventsFileSize `mod` pageSize /= 0
    then return $ Left $ "Corrupted events file: size " ++ show eventsFileSize ++ " is not a multiple of " ++ show pageSize
    else do
      let numPages = eventsFileSize `div` pageSize
      
      if numPages == 0
        then do
          -- Create first page with event
          let page = EventsPage.emptyPage
          case EventsPage.addEvent page eventData of
            Nothing -> return $ Left "Event too large for page"
            Just newPage -> do
              BS.writeFile eventsPath (EventsPage.toByteString newPage)
              return $ Right ()
        else do
          -- Read last page and try to add event
          let lastPageOffset = (numPages - 1) * pageSize
              lastPageData = BS.drop lastPageOffset eventsContent
          case EventsPage.fromByteString (BS.take pageSize lastPageData) of
            Left _ -> return $ Left "Corrupted last page"
            Right page -> do
              case EventsPage.addEvent page eventData of
                Just newPage -> do
                  -- Update last page
                  let beforeLastPage = BS.take lastPageOffset eventsContent
                      newContent = beforeLastPage <> EventsPage.toByteString newPage
                  BS.writeFile eventsPath newContent
                  return $ Right ()
                Nothing -> do
                  -- Page full, create new page and update index
                  let newPage = EventsPage.emptyPage
                  case EventsPage.addEvent newPage eventData of
                    Just finalPage -> do
                      -- Calculate the next offset for the new page
                      nextOffset <- calculateNextOffset indexPath eventsPath
                      
                      -- Append new events page
                      let newContent = eventsContent <> EventsPage.toByteString finalPage
                      BS.writeFile eventsPath newContent
                      
                      -- Update index with new entry
                      case IndexPage.fromByteString indexContent of
                        Left _ -> return $ Left "Corrupted index page"
                        Right indexPage -> do
                          case IndexPage.addEntry indexPage nextOffset of
                            Left _ -> return $ Left "Index full or invalid offset"
                            Right newIndexPage -> do
                              BS.writeFile indexPath (IndexPage.toByteString newIndexPage)
                              return $ Right ()
                    Nothing -> return $ Left "Event too large for page"

-- | Read events from file starting at given offset
readEventsFromFile :: FilePath -> Word64 -> IO [EventResponse]
readEventsFromFile path startOffset = do
  content <- BS.readFile path
  
  let pageSize = 8192
      numPages = BS.length content `div` pageSize
      
  -- Simple implementation: read all events and filter
  allEvents <- extractAllEvents content 0 numPages 0
  
  -- Safe conversion of Word64 offset to Int for drop
  let maxDrop :: Int
      maxDrop = maxBound
      safeDrop :: Int
      safeDrop =
        if startOffset > fromIntegral maxDrop
          then maxDrop
          else fromIntegral startOffset
  
  return $ drop safeDrop allEvents

-- | Extract all events from pages
extractAllEvents :: BS.ByteString -> Int -> Int -> Word64 -> IO [EventResponse]
extractAllEvents _ pageIdx numPages _ | pageIdx >= numPages = return []
extractAllEvents content pageIdx numPages cumulativeOffset = do
  let pageOffset = pageIdx * 8192
      pageData = BS.take 8192 $ BS.drop pageOffset content
  
  case EventsPage.fromByteString pageData of
    Left _ -> extractAllEvents content (pageIdx + 1) numPages cumulativeOffset
    Right page -> do
      let numEventsWord = EventsPage.eventCount page
          numEvents :: Int
          numEvents = fromIntegral numEventsWord
          eventsInPage =
            if numEventsWord == 0
              then []
              else map (extractEvent page cumulativeOffset) [0 .. numEvents - 1]
          nextOffset = cumulativeOffset + fromIntegral numEventsWord
      rest <- extractAllEvents content (pageIdx + 1) numPages nextOffset
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