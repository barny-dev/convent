module Web.Convent.Storage.EventStore where

import Data.ByteString (ByteString)
import System.IO (hSeek, hFileSize, SeekMode(..), hGet)

loadPage :: Int -> Handle -> IO ByteString
loadPage pageIdx h = do
  fileSize <- hFileSize h
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  hSeek h AbsoluteSeek offset
  hGet h pageSize