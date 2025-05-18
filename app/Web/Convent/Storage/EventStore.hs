
module Web.Convent.Storage.EventStore 
  ( readPage
  , writePage
  , FileReadError(..)
  ) where

import Web.Convent.Storage.EventsPage (EventsPage, PageReadError(..), fromByteString, toByteString)
import qualified System.IO as IO
import qualified Data.ByteString as ByteString

-- | Errors that can occur when reading pages from a file
data FileReadError =
    FileTooSmall Int Int                 -- ^ File is too small (expected size, actual size)
  | InvalidPageOffset Int                -- ^ Tried to read from invalid page offset
  | IOError IOError                      -- ^ Underlying IO operation failed
  | PageError PageReadError              -- ^ Error in page format/content
  deriving (Show, Eq)

-- | Reads a page from the event store at the specified index.
-- Returns an error if the file is too small or if the page data is invalid.
readPage :: IO.Handle -> Int -> IO (Either FileReadError EventsPage)
readPage h pageIdx = do
  if pageIdx < 0 
    then return $ Left $ InvalidPageOffset pageIdx
    else do
      fileSize <- IO.hFileSize h
      let pageSize = 8192
      let offset = fromIntegral pageIdx * pageSize
      let requiredSize = offset + pageSize
      if fileSize < requiredSize
        then return $ Left $ FileTooSmall (fromIntegral requiredSize) (fromIntegral fileSize)
        else do
          IO.hSeek h IO.AbsoluteSeek offset
          page <- ByteString.hGet h (fromIntegral pageSize)
          return $ case fromByteString page of
            Left err -> Left $ PageError err
            Right p -> Right p

-- | Writes a page to the event store at the specified index.
-- Will overwrite any existing data at that position.
writePage :: IO.Handle -> Int -> EventsPage -> IO ()
writePage h pageIdx page = do
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  do 
    IO.hSeek h IO.AbsoluteSeek offset
    ByteString.hPut h (toByteString page)
    return ()
