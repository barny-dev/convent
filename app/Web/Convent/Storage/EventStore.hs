module Web.Convent.Storage.EventStore where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified System.IO as IO
import Control.Exception (IOException)

data PageReadError =
  ReadFileTooSmall |
  ReadIOError IOException
  deriving (Show)

data PageWriteError = 
  WritePageSizeMismatch |
  WriteIOError IOException 
  deriving (Show)
readPage :: IO.Handle -> Int -> IO (Either PageReadError ByteString)
readPage h pageIdx= do
  fileSize <- IO.hFileSize h
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  if fileSize < offset + pageSize
    then return $ Left ReadFileTooSmall
    else do
      IO.hSeek h IO.AbsoluteSeek offset
      page <- ByteString.hGet h (fromIntegral pageSize)
      return $ Right page

writePage :: IO.Handle -> Int -> ByteString -> IO (Either PageWriteError ())
writePage h pageIdx page = do
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  if ByteString.length page /= pageSize
    then return $ Left WritePageSizeMismatch
    else do 
      IO.hSeek h IO.AbsoluteSeek (fromIntegral offset)
      ByteString.hPut h page
      return $ Right ()