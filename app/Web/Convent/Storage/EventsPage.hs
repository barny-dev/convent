
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

-- | Represents a page containing events with their offsets
newtype EventsPage = EventsPage ByteString deriving (Eq)

instance Show EventsPage where
  show page = "EventsPage { events: " ++ show (eventCount page) ++ ", reserve: " ++ show (reserve page) ++ " bytes }"

-- | Possible errors that can occur when reading a page
data PageReadError =
  InvalidPageSizeError Int |              -- ^ Page size is not 8192 bytes
  InvalidReservePtrError Word16 |         -- ^ Invalid reserve pointer value
  InvalidEventPtrError { 
    offendingPtr :: Word16,              -- ^ The invalid event pointer
    offendingPtrIndex :: Word16,         -- ^ Index of the invalid pointer
    offendingLimit :: Word16             -- ^ Upper/lower limit that was violated
  } |
  ReadFileTooSmallError                   -- ^ File is too small to read requested page
  deriving (Show, Eq)

-- | Creates an empty page with no events
emptyPage :: EventsPage
emptyPage = EventsPage . ByteString.toStrict . ByteString.Builder.toLazyByteString $ ByteString.Builder.word16BE 2 <> ByteString.Builder.lazyByteString (ByteString.Lazy.replicate 8190 0)

-- | Creates an EventsPage from a ByteString, validating the data format
-- Returns Left with an error if the data is invalid
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
    validateEventPtrs rptr limit ix ptrs = case ptrs of
          [] -> Right ()
          ptr:rest -> 
            if ptr >= limit then Left $ InvalidEventPtrError { offendingPtr = ptr, offendingPtrIndex = ix, offendingLimit = limit }
            else if ptr < rptr then Left $ InvalidEventPtrError { offendingPtr = ptr, offendingPtrIndex = ix, offendingLimit = rptr }
            else validateEventPtrs rptr ptr (ix + 1) rest

-- | Converts an EventsPage back to its raw ByteString representation
toByteString :: EventsPage -> ByteString
toByteString (EventsPage rawPage) = rawPage

-- | Returns the number of events in the page
eventCount :: EventsPage -> Word16
eventCount page = (reservePtr page - 2) `div` 2

-- | Returns the pointer to the event at the specified index
-- Returns Nothing if the index is out of bounds
eventPtr :: EventsPage -> Word16 -> Maybe Word16
eventPtr page ix =
  if eventCount page <= ix
  then Nothing
  else Just $ eventPtrUnsafe page ix

-- | Returns the event data at the specified index
-- Returns Nothing if the index is out of bounds
event :: EventsPage -> Word16 -> Maybe ByteString
event page ix =
  if eventCount page <= ix
  then Nothing
  else Just $ eventUnsafe page ix

-- | Returns a copy of the event data at the specified index
-- Returns Nothing if the index is out of bounds
eventCopy :: EventsPage -> Word16 -> Maybe ByteString
eventCopy page ix = fmap ByteString.copy (event page ix)

-- | Adds a new event to the page
-- Returns Nothing if there isn't enough space
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

-- | Returns the amount of free space in the page
reserve :: EventsPage -> Word16
reserve page =
  let c = eventCount page
      rptr = reservePtr page
      end = if c == 0 then 8192 else eventPtrUnsafe page (c - 1)
   in end - rptr

-- | Returns the current reserve pointer position
reservePtr :: EventsPage -> Word16
reservePtr (EventsPage rawPage) = readW16BE rawPage 0

-- | Internal function to get event pointer without bounds checking
eventPtrUnsafe :: EventsPage -> Word16 -> Word16
eventPtrUnsafe (EventsPage rawPage) ix =
  readW16BE rawPage $ (fromIntegral ix) * 2 + 2

-- | Internal function to get event data without bounds checking  
eventUnsafe :: EventsPage -> Word16 -> ByteString
eventUnsafe page ix =
  let start = fromIntegral $ eventPtrUnsafe page ix
      end = fromIntegral $ if ix == 0 then 8192 else eventPtrUnsafe page (ix - 1)
      len = end - start
   in ByteString.take len . ByteString.drop start $ toByteString page
