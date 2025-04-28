module Web.Convent.Storage.EventStore where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified System.IO as IO

data PageLoadError = FileTooSmall

loadPage :: Int -> IO.Handle -> IO (Either PageLoadError ByteString)
loadPage pageIdx h = do
  fileSize <- IO.hFileSize h
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  if fileSize < offset + pageSize
    then return $ Left FileTooSmall
    else do
      IO.hSeek h IO.AbsoluteSeek offset
      page <- ByteString.hGet h (fromIntegral pageSize)
      return $ Right page