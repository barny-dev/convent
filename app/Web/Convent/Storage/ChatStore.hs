
{-# LANGUAGE RecordWildCards #-}
module Web.Convent.Storage.ChatStore
  ( ChatStore
  , ChatData(..)
  , newChatStore
  , withChatLock
  , getHighestEventOffset
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import Data.Word (Word64)
import System.IO (Handle)
import Web.Convent.Storage.IndexPage
import Web.Convent.Storage.EventsPage
import Web.Convent.Storage.EventStore

-- | Represents data associated with a single chat
data ChatData = ChatData
  { indexHandle :: Handle            -- ^ Handle to the index file
  , eventsHandle :: Handle          -- ^ Handle to the events file
  , cachedIndexPages :: Map.Map Int IndexPage  -- ^ Currently loaded index pages
  , cachedEventPages :: Map.Map Int EventsPage -- ^ Currently loaded event pages
  }

-- | Thread-safe store of chat data
newtype ChatStore = ChatStore (MVar (Map.Map UUID (MVar ChatData)))

-- | Creates a new empty chat store
newChatStore :: IO ChatStore
newChatStore = ChatStore <$> newMVar Map.empty

-- | Executes an action with exclusive access to chat data
withChatLock :: ChatStore -> UUID -> (ChatData -> IO a) -> IO a
withChatLock (ChatStore mvar) chatId action = do
  store <- takeMVar mvar
  chatLock <- case Map.lookup chatId store of
    Just lock -> pure lock
    Nothing -> do
      lock <- newMVar undefined  -- Placeholder until chat is opened
      putMVar mvar (Map.insert chatId lock store)
      pure lock
  putMVar mvar store
  bracket
    (takeMVar chatLock)
    (putMVar chatLock)
    action

-- | Returns the highest event offset in a chat
getHighestEventOffset :: ChatStore -> UUID -> IO (Either String Word64)
getHighestEventOffset store chatId = withChatLock store chatId $ \ChatData{..} -> do
  -- Start from a high index and work backwards to find last non-empty page
  let findLastIndexPage n = 
        if n < 0 
        then return $ Left "No events found"
        else do
          result <- readPage indexHandle n
          case result of
            Left _ -> findLastIndexPage (n - 1)
            Right page -> 
              if entryCount page == 0
              then findLastIndexPage (n - 1)
              else return $ Right (n, page)
              
  indexResult <- findLastIndexPage 1024  -- Max possible index pages
  case indexResult of
    Left err -> return $ Left err
    Right (pageNum, indexPage) -> do
      let count = entryCount indexPage
          lastEntry = entry indexPage (count - 1)
          baseOffset = minimumEventOffset lastEntry
      -- Read the events page this entry points to
      eventsResult <- readPage eventsHandle pageNum
      case eventsResult of
        Left err -> return $ Left $ "Failed to read events page: " ++ show err
        Right eventsPage -> 
          return $ Right $ baseOffset + (fromIntegral $ eventCount eventsPage)
