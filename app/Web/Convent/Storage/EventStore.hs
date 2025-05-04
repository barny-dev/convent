module Web.Convent.Storage.EventStore where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified System.IO as IO
import Control.Exception (IOException)
import Data.Word (Word16)
import Data.Binary (Get, runGet, getWord16be)
import qualified Data.Binary.Get as Get

data PageReadError =
  ReadFileTooSmall |
  ReadIOError IOException
  deriving (Show)

data PageWriteError = 
  WritePageSizeMismatch |
  WriteIOError IOException 
  deriving (Show)

data PageParseError = InvalidReservePointer
  deriving (Show)

data Page = Page {
  reservePtr :: Word16,
  pointers :: [Word16],
  segments :: [ByteString]
} deriving (Show, Eq)

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

parsePage :: ByteString -> Either PageParseError Page
parsePage bs = runGet pageParser bs where
  pageParser :: Get (Either PageParseError Page)
  pageParser = do
    reserve <- getWord16be
    if reserve >= 8192 
      then return $ Left InvalidReservePointer
      else do
        ptrs <- parsePointers reserve []
        let segments = extractSegments bs (reverse ptrs) reserve
        return $ Right $ Page reserve (reverse ptrs) segments

  parsePointers :: Word16 -> [Word16] -> Get [Word16]
  parsePointers reserve acc = do
    remaining <- Get.remaining
    if remaining <= fromIntegral reserve 
      then return acc
      else do
        ptr <- getWord16be
        case acc of
          [] -> if ptr >= 8192 
                then return [] 
                else parsePointers reserve (ptr:acc)
          (prev:_) -> if ptr >= prev || ptr < reserve
                      then return acc
                      else parsePointers reserve (ptr:acc)

extractSegments :: ByteString -> [Word16] -> Word16 -> [ByteString]
extractSegments bs [] _ = []
extractSegments bs (ptr:ptrs) reserve = 
  case ptrs of
    [] -> [ByteString.drop (fromIntegral ptr) bs]
    (nextPtr:_) -> ByteString.take (fromIntegral nextPtr - fromIntegral ptr) 
                     (ByteString.drop (fromIntegral ptr) bs) 
                     : extractSegments bs ptrs reserve