
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Web.Convent.Storage.ChatStore
  ( ChatStore
  , ChatStoreConfig(..)
  , ChatData
  , newChatStore
  , loadExistingChats
  , createChat
  , chatExists
  , joinChatParticipant
  , postChatMessage
  , getChatEvents
  , EventResponse
  ) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Data.Word (Word64)
import Data.Text (Text)
import System.IO (openBinaryFile, hClose, IOMode(..))
import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import qualified Web.Convent.Storage.ChatFileOps as ChatFileOps
import Web.Convent.Storage.ChatDataOps
import qualified Web.Convent.Events.ParticipantJoinedEvent as ParticipantJoinedEvent
import qualified Web.Convent.Events.MessageSubmittedEvent as MessageSubmittedEvent

-- | Configuration for ChatStore
data ChatStoreConfig = ChatStoreConfig
  { chatsDirectory :: FilePath  -- ^ Base directory for storing chat data
  } deriving (Show, Eq)

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

-- | Load all existing chats from disk into memory
-- Should be called at startup to populate the store
loadExistingChats :: ChatStore -> IO (Either String Int)
loadExistingChats store@(ChatStore dataMVar config) = runExceptT $ do
  let baseDir = chatsDirectory config
  
  -- Check if base directory exists
  baseDirExists <- lift $ doesDirectoryExist baseDir
  if not baseDirExists
    then return 0  -- No chats to load
    else do
      -- List all directories in the chats directory
      entries <- lift $ listDirectory baseDir
      
      -- Try to load each directory as a chat
      let loadChat dirName = do
            case UUID.fromString dirName of
              Nothing -> return Nothing  -- Not a valid UUID, skip
              Just uuid -> do
                let chatDir = baseDir ++ "/" ++ dirName
                    indexPath = chatDir ++ "/index.dat"
                    eventsPath = chatDir ++ "/events.dat"
                
                -- Check if both required files exist
                indexExists <- doesFileExist indexPath
                eventsExists <- doesFileExist eventsPath
                
                if indexExists && eventsExists
                  then do
                    -- Open handles
                    indexHandle <- openBinaryFile indexPath ReadWriteMode
                    eventsHandle <- openBinaryFile eventsPath ReadWriteMode
                    
                    -- Initialize ChatData
                    eitherChatData <- ChatFileOps.initChatDataFromFiles indexHandle eventsHandle
                    
                    case eitherChatData of
                      Left _ -> do
                        -- Failed to load, close handles
                        hClose indexHandle
                        hClose eventsHandle
                        return Nothing
                      Right chatData -> do
                        -- Create MVar and add to store
                        chatDataMVar <- newMVar chatData
                        modifyMVar_ dataMVar $ \chatsMap ->
                          return $ Map.insert uuid chatDataMVar chatsMap
                        return $ Just uuid
                  else return Nothing  -- Missing files
      
      -- Load all chats
      maybeUUIDs <- lift $ mapM loadChat entries
      let loadedUUIDs = [uuid | Just uuid <- maybeUUIDs]
      
      return $ length loadedUUIDs

-- | Creates a new chat with a unique UUID and initializes storage files
-- The chat is automatically loaded into memory and ready for use
createChat :: ChatStore -> IO UUID
createChat (ChatStore dataMVar config) = do
  -- Generate a new UUID for the chat
  uuid <- UUID.V4.nextRandom
  
  -- Create chat directory using configured base path
  let chatDir = chatsDirectory config ++ "/" ++ UUID.toString uuid
  
  -- Use low-level file operations to create chat files
  ChatFileOps.createChatFiles chatDir
  
  -- Load the new chat into memory (open handles and initialize ChatData)
  let indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  indexHandle <- openBinaryFile indexPath ReadWriteMode
  eventsHandle <- openBinaryFile eventsPath ReadWriteMode
  eitherChatData <- ChatFileOps.initChatDataFromFiles indexHandle eventsHandle
  
  case eitherChatData of
    Left _ -> do
      -- Failed to initialize, close handles
      hClose indexHandle
      hClose eventsHandle
      error "Failed to initialize newly created chat"
    Right chatData -> do
      -- Add to in-memory cache
      chatDataMVar <- newMVar chatData
      modifyMVar_ dataMVar $ \chatsMap -> do
        return $ Map.insert uuid chatDataMVar chatsMap
      
      return uuid

-- | Check if a chat exists - uses in-memory cache for active chats
chatExists :: ChatStore -> UUID -> IO Bool
chatExists (ChatStore dataMVar _) uuid = do
  -- Check if chat is active in memory (fast path)
  chatsMap <- readMVar dataMVar
  return $ Map.member uuid chatsMap

-- | Execute an action with ChatData for an active chat
-- IMPORTANT: Chat must already be loaded into memory (via loadExistingChats or createChat)
-- Throws an error if chat doesn't exist in memory
withChatData :: ChatStore -> UUID -> (ChatData -> IO (Either String (ChatData, a))) -> IO (Either String a)
withChatData (ChatStore dataMVar _) uuid action = do
  -- Get MVar for this chat (must already exist)
  chatsMap <- readMVar dataMVar
  case Map.lookup uuid chatsMap of
    Nothing -> return $ Left "Chat does not exist"  -- Chat not loaded
    Just chatDataMVar -> do
      -- Chat exists, work with it
      chatData <- takeMVar chatDataMVar
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
      Right events -> return $ Right (chatData, events)
-- ChatData unchanged for reads