
module Web.Convent.Storage.EventsPage 
  ( EventsPage()
  , emptyPage
  , PageReadError(..)
  , fromByteString
  , toByteString
  , eventCount
  , eventPtr
  , event
  , eventCopy
  , addEvent
  , reserve
  , reservePtr
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Word (Word16)
import Web.Convent.Util.ByteString (readW16BE)

newtype EventsPage = EventsPage ByteString deriving (Eq)

instance Show EventsPage where
  show page = "EventsPage { events: " ++ show (eventCount page) ++ ", reserve: " ++ show (reserve page) ++ " bytes }"

data PageReadError =
  InvalidPageSizeError Int |
  InvalidReservePtrError Word16 |
  InvalidEventPtrError { offendingPtr :: Word16, offendingPtrIndex :: Word16, offendingLimit :: Word16 } deriving (Show, Eq)

fromByteString :: ByteString -> Either PageReadError EventsPage
fromByteString rawPage = do
  let pageSize = ByteString.length rawPage
  (InvalidPageSizeError pageSize) `whenNot` (pageSize == 8192)
  let rptr = readW16BE rawPage 0
  (InvalidReservePtrError rptr) `whenNot` (rptr < 8192 && rptr >= 2 && rptr `mod` 2 == 0)
  let eventPtrs = map (readW16BE rawPage) [2,4..((fromIntegral rptr) - 2)]
  validateEventPtrs rptr 8192 0 eventPtrs
  return $ EventsPage rawPage
  where
    whenNot err cond = if cond then Right () else Left err
    validateEventPtrs rptr limit ix ptrs  = case ptrs of
          [] -> Right ()
          ptr:rest -> 
            if ptr >= limit then Left $ InvalidEventPtrError { offendingPtr = ptr, offendingPtrIndex = ix, offendingLimit = limit }
            else if ptr < rptr then Left $ InvalidEventPtrError { offendingPtr = ptr, offendingPtrIndex = ix, offendingLimit = rptr }
            else validateEventPtrs rptr ptr (ix + 1) rest

toByteString :: EventsPage -> ByteString
toByteString (EventsPage rawPage) = rawPage

emptyPage :: EventsPage
emptyPage = EventsPage . ByteString.toStrict . ByteString.Builder.toLazyByteString $ ByteString.Builder.word16BE 2 <> ByteString.Builder.lazyByteString (ByteString.Lazy.replicate 8190 0)

addEvent :: EventsPage -> ByteString -> Maybe EventsPage
addEvent page@(EventsPage rawPage) evt =
  if (fromIntegral $ reserve page) < ByteString.length evt + 2 then Nothing
  else let oldRptr = reservePtr page
           newRptr = oldRptr + 2
           oldCount = eventCount page
           lastPtr = if oldCount == 0 then 8192 else eventPtrUnsafe page (oldCount - 1)
           newPtr = lastPtr - (fromIntegral $ ByteString.length evt)
        in Just . EventsPage . ByteString.toStrict . ByteString.Builder.toLazyByteString $
          ByteString.Builder.word16BE newRptr <>
          ByteString.Builder.byteString (ByteString.take (fromIntegral oldRptr - 2) (ByteString.drop 2 rawPage)) <>
          ByteString.Builder.word16BE newPtr <>
          ByteString.Builder.lazyByteString (ByteString.Lazy.replicate (fromIntegral newPtr - fromIntegral newRptr) 0) <>
          ByteString.Builder.byteString evt <>
          ByteString.Builder.byteString (ByteString.drop (fromIntegral lastPtr) rawPage)

reserve :: EventsPage -> Word16
reserve page =
  let c = eventCount page
      rptr = reservePtr page
      end = if c == 0 then 8192 else eventPtrUnsafe page (c - 1)
   in end - rptr

reservePtr :: EventsPage -> Word16
reservePtr (EventsPage rawPage) = readW16BE rawPage 0

eventCount :: EventsPage -> Word16
eventCount page = (reservePtr page - 2) `div` 2

eventCopy :: EventsPage -> Word16 -> Maybe ByteString
eventCopy page ix = fmap ByteString.copy (event page ix)

event :: EventsPage -> Word16 -> Maybe ByteString
event page ix =
  if eventCount page <= ix
  then Nothing
  else Just $ eventUnsafe page ix

eventUnsafe :: EventsPage -> Word16 -> ByteString
eventUnsafe page ix =
  let start = fromIntegral $ eventPtrUnsafe page ix
      end = fromIntegral $ if ix == 0 then 8192 else eventPtrUnsafe page (ix - 1)
      len = end - start
   in ByteString.take len . ByteString.drop start $ toByteString page

eventPtr :: EventsPage -> Word16 -> Maybe Word16
eventPtr page ix =
  if eventCount page <= ix
  then Nothing
  else Just $ eventPtrUnsafe page ix
  
eventPtrUnsafe :: EventsPage -> Word16 -> Word16
eventPtrUnsafe (EventsPage rawPage) ix =
  readW16BE rawPage $ (fromIntegral ix) * 2 + 2


