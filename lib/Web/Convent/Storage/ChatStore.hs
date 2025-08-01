
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
import Data.Maybe (maybe)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import Web.Convent.Storage.EventStore
import qualified Web.Convent.Storage.FilePage as FilePage

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
getHighestEventOffset store chatId = withChatLock store chatId $
  \ChatData { indexPageCount = ixPgCnt, cachedIndexPages = ixPgs, cachedEventPages = evPgs, .. } -> runExceptT $ do
    lastIndexPageOffset <- if ixPgCnt < 1 then throwE "illegal: missing index" else return (ixPgCnt - 1)
    lastIndexPage <- maybe (throwE "illegal: last index page not loaded") return (Map.lookup lastIndexPageOffset ixPgs)
    let indexPageEntryCount = IndexPage.entryCount lastIndexPage
    (eventPageOffset, baseEventOffset) <- case (lastIndexPageOffset, indexPageEntryCount) of
      (0, 0) -> return (0, 0)
      (n, 0) -> throwE "illegal: empty last index page"
      _ -> let lastEntry = IndexPage.entry lastIndexPage (indexPageEntryCount - 1)
            in return (indexPageEntryCount - 1 + (lastIndexPageOffset * 1024), IndexPage.minimumEventOffset lastEntry)
    lastEventPage <- maybe (throwE "illegal: last event page not loaded") return (Map.lookup eventPageOffset evPgs)
    return . fromIntegral $ baseEventOffset + (fromIntegral $ EventsPage.eventCount lastEventPage) - 1