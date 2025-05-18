
module Web.Convent.Storage.IndexPage
  ( IndexPage()
  , IndexPageError(..)
  , fromByteString
  , toByteString
  , indexSegmentCount
  , indexSegments
  , indexSegment
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word64)
import Data.Int (Int64)
import Data.Bits (shiftL)

newtype IndexPage = IndexPage ByteString deriving (Eq)

instance Show IndexPage where
  show page = "IndexPage { segments: " ++ show (indexSegmentCount page) ++ " }"

data IndexPageError =
  InvalidPageSizeError Int |
  InvalidSegmentError Int |
  NonZeroTrailingSegmentError Int |
  NonAscendingOffsetError Int |
  NonAscendingEventOffsetError Int
  deriving (Show, Eq)

fromByteString :: ByteString -> Either IndexPageError IndexPage
fromByteString rawPage = do
  let pageSize = ByteString.length rawPage
  (InvalidPageSizeError pageSize) `whenNot` (pageSize == 8192)
  validateSegments rawPage 0 (0, 0)
  return $ IndexPage rawPage
  where
    whenNot err cond = if cond then Right () else Left err
    validateSegments bs ix (lastPage, lastEvent) =
      if ix >= 64 then Right () -- 8192 / 128 = 64 segments total
      else do
        let offset = ix * 16
        let pageOffset = readW64BE bs offset
        let eventOffset = readW64BE bs (offset + 8)
        if pageOffset == 0 && eventOffset == 0
          then validateTrailingZeros bs (ix + 1)
          else do
            (NonAscendingOffsetError ix) `whenNot` (pageOffset > lastPage)
            (NonAscendingEventOffsetError ix) `whenNot` (eventOffset > lastEvent)
            validateSegments bs (ix + 1) (pageOffset, eventOffset)
    validateTrailingZeros bs ix =
      if ix >= 64 then Right ()
      else do
        let offset = ix * 16
        let pageOffset = readW64BE bs offset
        let eventOffset = readW64BE bs (offset + 8)
        (NonZeroTrailingSegmentError ix) `whenNot` (pageOffset == 0 && eventOffset == 0)
        validateTrailingZeros bs (ix + 1)

toByteString :: IndexPage -> ByteString
toByteString (IndexPage rawPage) = rawPage

indexSegmentCount :: IndexPage -> Int
indexSegmentCount (IndexPage rawPage) = count 0
  where
    count ix = 
      if ix >= 64 then 0
      else let offset = ix * 16
               pageOffset = readW64BE rawPage offset
               eventOffset = readW64BE rawPage (offset + 8)
           in if pageOffset == 0 && eventOffset == 0 
              then 0 
              else 1 + count (ix + 1)

data IndexSegment = IndexSegment {
  pageOffset :: Word64,
  minimumEventOffset :: Word64
} deriving (Show, Eq)

indexSegments :: IndexPage -> [IndexSegment]
indexSegments page = 
  [ segment | ix <- [0..63], 
    let segment = indexSegment page ix, 
    pageOffset segment /= 0 || minimumEventOffset segment /= 0 ]

indexSegment :: IndexPage -> Int -> IndexSegment
indexSegment (IndexPage rawPage) ix =
  let offset = ix * 16
      pOffset = readW64BE rawPage offset
      eOffset = readW64BE rawPage (offset + 8)
   in IndexSegment { pageOffset = pOffset, minimumEventOffset = eOffset }

readW64BE :: ByteString -> Int -> Word64
readW64BE bs offset =
  let b0 = fromIntegral (ByteString.index bs offset) `shiftL` 56
      b1 = fromIntegral (ByteString.index bs (offset + 1)) `shiftL` 48
      b2 = fromIntegral (ByteString.index bs (offset + 2)) `shiftL` 40
      b3 = fromIntegral (ByteString.index bs (offset + 3)) `shiftL` 32
      b4 = fromIntegral (ByteString.index bs (offset + 4)) `shiftL` 24
      b5 = fromIntegral (ByteString.index bs (offset + 5)) `shiftL` 16
      b6 = fromIntegral (ByteString.index bs (offset + 6)) `shiftL` 8
      b7 = fromIntegral (ByteString.index bs (offset + 7))
   in b0 + b1 + b2 + b3 + b4 + b5 + b6 + b7
