
{-# LANGUAGE RecordWildCards #-}
module Web.Convent.Storage.ChatStore
  ( ChatStore
  , ChatData(..)
  , newChatStore
  , withChatLock
  , getHighestEventOffset
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT (..), except, withExceptT, throwE, catchE, runExceptT)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import Data.Word (Word64)
import System.IO (Handle, hFileSize)
import Web.Convent.Storage.IndexPage (IndexPage)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage
import qualified Web.Convent.Storage.EventsPage as EventsPage
import Web.Convent.Storage.EventStore

-- | Represents data associated with a single chat
data ChatData = ChatData
  { indexHandle :: Handle            -- ^ Handle to the index file
  , eventsHandle :: Handle          -- ^ Handle to the events file
  , cachedIndexPages :: Map.Map Int IndexPage  -- ^ Currently loaded index pages
  , cachedEventPages :: Map.Map Int EventsPage -- ^ Currently loaded event pages
  , indexPageCount :: Int            -- ^ Number of index pages
  , eventsPageCount :: Int           -- ^ Number of events pages
  }

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
    maybeLastIndexPage <- if indexPageCount > 0
                          then withExceptT (const IndexFileCorrupted) $
                               fmap Just . ExceptT $
                               IndexPage.readPage indexHandle (indexPageCount - 1)
                          else return Nothing
  -- Read last events page if available
    maybeLastEventsPage <- if eventsPageCount > 0
                           then withExceptT (const EventsFileCorrupted) $
                                fmap Just . ExceptT $
                                EventsPage.readPage eventsHandle (eventsPageCount - 1)
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

-- | Returns the highest event offset in a chat. TODO: this is AI generated garbage, fix it.
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
