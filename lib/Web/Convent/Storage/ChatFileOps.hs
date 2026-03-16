{-# LANGUAGE RecordWildCards #-}
-- | Low-level file operations for chat storage
-- Handles direct file creation and initialization
module Web.Convent.Storage.ChatFileOps
  ( createChatFiles
  , initChatDataFromFiles
  , ChatFileError(..)
  ) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Trans.Class (lift)
import System.IO (Handle, hFileSize)
import qualified System.IO as IO
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map.Strict as Map
import Web.Convent.Storage.IndexPage (IndexPage)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import qualified Web.Convent.Storage.FilePage as FilePage
import Web.Convent.Storage.ChatDataOps (ChatData(..))

-- | Errors that can occur during file operations
data ChatFileError = IndexFileCorrupted | EventsFileCorrupted
  deriving (Show, Eq)

-- | Create chat storage files with initial empty pages
createChatFiles :: FilePath -> IO ()
createChatFiles chatDir = do
  -- Create chat directory
  createDirectoryIfMissing True chatDir
  
  -- Create empty index and events files
  let indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"

  IO.withBinaryFile indexPath IO.WriteMode $ \indexHandle -> do
    either (error . show) (const $ return ()) =<<
      FilePage.write indexHandle (FilePage.Index 0, FilePage.Size 8192) (IndexPage.toByteString IndexPage.emptyPage)
  IO.withBinaryFile eventsPath IO.WriteMode $ \eventsHandle -> do
    either (error . show) (const $ return ()) =<<
      FilePage.write eventsHandle (FilePage.Index 0, FilePage.Size 8192) (EventsPage.toByteString EventsPage.emptyPage)

-- | Initialize ChatData from file handles
-- Reads file sizes, calculates page counts, and caches the last page of each file
initChatDataFromFiles :: Handle -> Handle -> IO (Either ChatFileError ChatData)
initChatDataFromFiles indexHandle eventsHandle = runExceptT $ do
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
