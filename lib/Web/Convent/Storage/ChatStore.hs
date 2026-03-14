
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Web.Convent.Storage.ChatStore
  ( ChatStore
  , ChatStoreConfig(..)
  , ChatData(..)
  , newChatStore
  , createChat
  , withChatData
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
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import System.IO (Handle, hFileSize, openFile, hClose, IOMode(..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.Convent.Storage.IndexPage (IndexPage)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import Web.Convent.Storage.EventStore
import qualified Web.Convent.Storage.FilePage as FilePage
import qualified Web.Convent.Events.ParticipantJoinedEvent as ParticipantJoinedEvent
import qualified Web.Convent.Events.MessageSubmittedEvent as MessageSubmittedEvent

-- | Represents data associated with a single chat
data ChatData = ChatData
  { chatDataIndexHandle :: Handle            -- ^ Handle to the index file
  , chatDataEventsHandle :: Handle          -- ^ Handle to the events file
  , chatDataCachedIndexPages :: Map.Map Int IndexPage  -- ^ Currently loaded index pages
  , chatDataCachedEventPages :: Map.Map Int EventsPage -- ^ Currently loaded event pages
  , chatDataIndexPageCount :: Int            -- ^ Number of index pages
  , chatDataEventsPageCount :: Int           -- ^ Number of events pages
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

-- | Thread-safe store of active chat data with configuration
data ChatStore = ChatStore 
  { chatStoreData :: MVar (Map.Map UUID (MVar ChatData))
  , chatStoreConfig :: ChatStoreConfig
  }

-- | Creates a new empty chat store with configuration
newChatStore :: ChatStoreConfig -> IO ChatStore
newChatStore config = do
  chatsMap <- newMVar Map.empty
  return $ ChatStore chatsMap config

-- | Initialize ChatData from file handles
initChatData :: Handle -> Handle -> IO (Either ChatStoreError ChatData)
initChatData indexHandle eventsHandle = runExceptT $ do
  -- Get file sizes for index and events files
  indexSize <- lift $ hFileSize indexHandle
  eventsSize <- lift $ hFileSize eventsHandle
  
  -- Calculate page counts
  let indexPageCount = fromIntegral (indexSize `div` 8192)
      eventsPageCount = fromIntegral (eventsSize `div` 8192)  
      
  -- Read last index page if available
  maybeLastIndexPage :: Maybe IndexPage <- 
    if indexPageCount > 0
      then withExceptT (const IndexFileCorrupted) $
           fmap Just . ExceptT $
           FilePage.load indexHandle (FilePage.Index (indexPageCount - 1), FilePage.Size 8192)
      else return Nothing
      
  -- Read last events page if available
  maybeLastEventsPage :: Maybe EventsPage <- 
    if eventsPageCount > 0
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
  return ChatData 
    { chatDataIndexHandle = indexHandle
    , chatDataEventsHandle = eventsHandle
    , chatDataCachedIndexPages = cachedIndexPages
    , chatDataCachedEventPages = cachedEventPages
    , chatDataIndexPageCount = indexPageCount
    , chatDataEventsPageCount = eventsPageCount
    }

-- | Creates a new chat with a unique UUID and initializes storage files
createChat :: ChatStore -> IO UUID
createChat (ChatStore _ config) = do
  -- Generate a new UUID for the chat
  uuid <- UUID.V4.nextRandom
  
  -- Create chat directory using configured base path
  let chatDir = chatsDirectory config ++ "/" ++ UUID.toString uuid
  createDirectoryIfMissing True chatDir
  
  -- Create empty index and events files
  let indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  -- Create files with initial empty page
  BS.writeFile indexPath (IndexPage.toByteString IndexPage.emptyPage)
  BS.writeFile eventsPath (EventsPage.toByteString EventsPage.emptyPage)
  
  return uuid

-- | Check if a chat exists - uses in-memory cache for active chats, falls back to file system
chatExists :: ChatStore -> UUID -> IO Bool
chatExists store@(ChatStore dataMVar config) uuid = do
  -- First check if chat is active in memory
  chatsMap <- readMVar dataMVar
  case Map.lookup uuid chatsMap of
    Just _ -> return True  -- Chat is active in memory
    Nothing -> do
      -- Check file system
      let chatDir = chatsDirectory config ++ "/" ++ UUID.toString uuid
      doesDirectoryExist chatDir

-- | Execute an action with ChatData, opening and closing it as needed
-- The action receives ChatData and can return an updated ChatData along with the result
withChatData :: ChatStore -> UUID -> (ChatData -> IO (Either String (ChatData, a))) -> IO (Either String a)
withChatData store@(ChatStore dataMVar config) uuid action = do
  -- Get or create MVar for this chat
  chatDataMVar <- modifyMVar dataMVar $ \chatsMap ->
    case Map.lookup uuid chatsMap of
      Just mvar -> return (chatsMap, mvar)
      Nothing -> do
        mvar <- newEmptyMVar
        return (Map.insert uuid mvar chatsMap, mvar)
  
  -- Try to take from MVar (chat data is already loaded)
  maybeChatData <- tryTakeMVar chatDataMVar
  
  case maybeChatData of
    Just chatData -> do
      -- Chat data already loaded, use it
      result <- action chatData
      case result of
        Left err -> do
          -- Put back original on error
          putMVar chatDataMVar chatData
          return $ Left err
        Right (newChatData, value) -> do
          -- Put back updated ChatData
          putMVar chatDataMVar newChatData
          return $ Right value
    Nothing -> do
      -- Need to load chat data from disk
      let chatDir = chatsDirectory config ++ "/" ++ UUID.toString uuid
          indexPath = chatDir ++ "/index.dat"
          eventsPath = chatDir ++ "/events.dat"
      
      -- Open file handles
      indexHandle <- openFile indexPath ReadWriteMode
      eventsHandle <- openFile eventsPath ReadWriteMode
      
      -- Initialize ChatData
      eitherChatData <- initChatData indexHandle eventsHandle
      
      case eitherChatData of
        Left err -> do
          -- Clean up handles on error
          hClose indexHandle
          hClose eventsHandle
          return $ Left $ "Failed to initialize chat data: " ++ show err
        Right chatData -> do
          -- Execute action with chat data
          result <- action chatData
          case result of
            Left err -> do
              -- Put back original on error
              putMVar chatDataMVar chatData
              return $ Left err
            Right (newChatData, value) -> do
              -- Put back updated ChatData
              putMVar chatDataMVar newChatData
              return $ Right value

-- Mid-level operations on ChatData

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

-- High-level operations for Web API

-- | Join a chat as a participant
joinChatParticipant :: ChatStore -> UUID -> Text -> IO (Either String (Word64, Word64))
joinChatParticipant store uuid participantName = 
  withChatData store uuid $ \chatData -> runExceptT $ do
    -- Generate participant ID with microsecond precision
    timestamp <- lift getCurrentTimestamp
    let pid = timestamp
    
    -- Create ParticipantJoinedEvent
    let event = ParticipantJoinedEvent.Event
          { ParticipantJoinedEvent.participantId = pid
          , ParticipantJoinedEvent.timestamp = timestamp
          , ParticipantJoinedEvent.participantName = participantName
          }
    
    -- Add event using mid-level operation
    result <- lift $ addEventToChatData chatData (ParticipantJoinedEvent.encode event)
    (newChatData, eventOffset) <- except result
    return (newChatData, (pid, eventOffset))

-- | Post a message to a chat
postChatMessage :: ChatStore -> UUID -> Word64 -> Text -> IO (Either String Word64)
postChatMessage store uuid participantId messageText = 
  withChatData store uuid $ \chatData -> runExceptT $ do
    -- Get current timestamp
    timestamp <- lift getCurrentTimestamp
    
    -- Create MessageSubmittedEvent
    let event = MessageSubmittedEvent.Event
          { MessageSubmittedEvent.participantId = participantId
          , MessageSubmittedEvent.timestamp = timestamp
          , MessageSubmittedEvent.message = messageText
          }
    
    -- Add event using mid-level operation
    result <- lift $ addEventToChatData chatData (MessageSubmittedEvent.encode event)
    (newChatData, eventOffset) <- except result
    return (newChatData, eventOffset)

-- | Get events from a chat starting from an offset
getChatEvents :: ChatStore -> UUID -> Word64 -> IO (Either String [EventResponse])
getChatEvents store uuid startOffset =
  withChatData store uuid $ \chatData -> do
    result <- getEventsFromChatData chatData startOffset
    case result of
      Left err -> return $ Left err
      Right events -> return $ Right (chatData, events)  -- ChatData unchanged for reads