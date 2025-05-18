
module Web.Convent.Storage.EventStore 
  ( readPage
  , writePage
  ) where

import Web.Convent.Storage.EventsPage (EventsPage, PageReadError(..), fromByteString, toByteString)
import qualified System.IO as IO
import qualified Data.ByteString as ByteString

-- | Reads a page from the event store at the specified index.
-- Returns an error if the file is too small or if the page data is invalid.
readPage :: IO.Handle -> Int -> IO (Either PageReadError EventsPage)
readPage h pageIdx = do
  fileSize <- IO.hFileSize h
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  if fileSize < offset + pageSize
    then return $ Left ReadFileTooSmallError
    else do
      IO.hSeek h IO.AbsoluteSeek offset
      page <- ByteString.hGet h (fromIntegral pageSize)
      return $ fromByteString page

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
