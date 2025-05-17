module Web.Convent.Storage.EventStore 
  ( Page()
  , parsePage
  , segment
  , segmentCopy
  , addSegment
  , reserve
  , readPage
  , writePage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified System.IO as IO
import Control.Exception (IOException)
import Data.Word (Word16)

newtype Page = Page ByteString deriving (Eq)

data PageReadError =
  ReadFileTooSmallError |
  InvalidPageSizeError Int |
  InvalidReservePtrError Word16 |
  InvalidSegmentPtrError { offendingPtr :: Word16, offendingPtrIndex :: Word16, offendingLimit :: Word16 } deriving (Show, Eq)

fromByteString :: ByteString -> Either PageReadError Page
parsePage rawPage = do
  let pageSize = ByteString.length rawPage
  InvalidPageSizeError pageSize `whenNot` pageSize == 8192
  let reservePtr = readW16BE rawPage 0
  InvalidReservePtrError reservePtr `whenNot` reservePtr < 8192 && reservePtr >= 2 && reservePtr `mod` 2 == 0
  let segmentPtrs = map (readW16BE rawPage) [2,4..(reservePtr - 2)]
  validateSegmentPtrs reservePtr 8192 0 segmentPtrs
  return $ Page rawPage
  where whenNot err cond = if cond then Right () else Left err
        readW16BE bs offset = (ByteString.index bs (fromIntegral offset) * 256 + ByteString.index bs (fromIntegral $ offset + 1))
        validateSegmentPtrs rptr limit ix ptrs  = case ptrs of
          [] -> Right ()
          ptr:rest -> 
            if ptr >= limit then Left $ InvalidSegmentPtrError { offendingPtr = ptr, offendingPtrIndex = ix, offendingLimit = limit }
            else if ptr < rptr then Left $ InvalidSegmentPtrError { offendingPtr = ptr, offendingPtrIndex = ix, offendingLimit = rptr }
            else findInvalidPtr rptr ptr (ix + 1) rest

toByteString :: Page -> ByteString
toByteString (Page rawPage) = rawPage

emptyPage :: Page
emptyPage = Page $ ByteString.Builder.word16BE 2 <> ByteString.Builder.lazyBytestring (ByteString.Lazy.replicate 8190 0)

addSegment :: Page -> ByteString -> Maybe Page
addSegment page@(Page rawPage) segment =
  if reserve page < ByteString.length segment + 2 then Nothing
  else let oldRptr = reservePtr page
           newRptr = rptr + 2
           oldCount = segmentCount page
           lastSptr = if oldCount == 0 then 8192 else segmentPtrUnsafe page (oldCount - 1)
           newSptr = lastSptr - ByteString.length segment
        in Just . Page $
          ByteString.Builder.word16BE newRptr <>
          ByteString.Builder.bytestring (ByteString.take (fromIntegral oldRptr - 2) (ByteString.drop 2 rawPage)) <>
          ByteString.Builder.word16BE newSptr <>
          ByteString.Builder.lazyBytestring (ByteString.Lazy.replicate (fromIntegral newSptr - fromIntegral newRptr) 0) <>
          ByteString.Builder.bytestring segment <>
          ByteString.Builder.bytestring (ByteString.drop (fromIntegral lastSptr) rawPage)

reserve :: Page -> Word16
reserve page =
  let c = segmentCount page
      rptr = reservePtr page
      end = if c == 0 then 8192 else segmentPtrUnsafe page (c - 1)
   in end - rptr

reservePtr :: Page -> Word16
reservePtr (Page rawPage) = readW16BE rawPage 0

segmentCount :: Page -> Word16
segmentCount page = (reservePtr page - 2) `div` 2

segmentCopy :: Page -> Word16 -> Maybe ByteString
segmentCopy page ix = fmap ByteString.copy (segment page)

segment :: Page -> Word16 -> Maybe ByteString
segment page ix =
  if segmentCount page < ix
  then Nothing
  else Just $ segmentUnsafe page

segmentUnsafe :: Page -> Word16 -> ByteString
segmentUnsafe (Page rawPage) ix =
  let start = segmentPtrUnsafe page ix
      end = if ix == 0 then 8192 else segmentPtrUnsafe page (ix - 1)
      len = end - start
   in ByteString.take len . ByteString.drop start $ rawPage

segmentPtr :: Page -> Word16 -> Maybe Word16
segmentPtr page ix =
  if segmentCount page < ix
  then Nothing
  else Just $ segmentPtrUnsafe page ix
  
segmentPtrUnsafe :: Page -> Word16 -> Word16
segmentPtrUnsafe (Page rawPage) ix =
  readW16BE rawPage $ (fromIntegral ix) * 2 + 2

readW16BE rawPage $ (fromIntegral ix) * 2 + 2)

readW16BE :: ByteString -> Int -> Word16
readW16BE bs offset = (ByteString.index bs offset) * 256 + ByteString.index bs (offset + 1)

readPage :: IO.Handle -> Int -> IO (Either PageReadError Page)
readPage h pageIdx = do
  fileSize <- IO.hFileSize h
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  if fileSize < offset + pageSize
    then return $ Left ReadFileTooSmallError
    else do
      IO.hSeek h IO.AbsoluteSeek offset
      page <- ByteString.hGet h (fromIntegral pageSize)
      fromByteString page

writePage :: IO.Handle -> Int -> Page -> IO ()
writePage h pageIdx page = do
  let pageSize = 8192
  let offset = fromIntegral pageIdx * pageSize
  do 
    IO.hSeek h IO.AbsoluteSeek (fromIntegral offset)
    ByteString.hPut h (toByteString page)
    return ()
