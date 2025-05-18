module Web.Convent.Storage.EventStore 
  ( readPage
  , writePage
  ) where

import Web.Convent.Storage.EventsPage (EventsPage, PageReadError(..), fromByteString, toByteString)
import qualified System.IO as IO
import qualified Data.ByteString as ByteString

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

writePage :: IO.Handle -> Int -> EventsPage -> IO ()
writePage h pageIdx page = do
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  do 
    IO.hSeek h IO.AbsoluteSeek offset
    ByteString.hPut h (toByteString page)
    return ()