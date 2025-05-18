module Web.Convent.Storage.IndexPage
  ( IndexPage()
  , IndexPageError(..)
  , fromByteString
  , toByteString
  , entryCount
  , entries
  , entry
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word64)
import Data.Int (Int64)
import Web.Convent.Util.ByteString (readW64BE)

-- | Represents a page in the index containing segment information
newtype IndexPage = IndexPage ByteString deriving (Eq)

instance Show IndexPage where
  show page = "IndexPage { segments: " ++ show (entryCount page) ++ " }"

-- | Possible errors that can occur when reading an index page
data IndexPageError =
  InvalidPageSizeError Int |      -- ^ Page size is not 8192 bytes
  InvalidEntryError Int |         -- ^ Entry data is invalid
  NonZeroTrailingEntryError Int | -- ^ Found non-zero entry after a zero entry
  NonAscendingOffsetError Int |   -- ^ Page offsets are not strictly ascending
  NonAscendingEventOffsetError Int -- ^ Event offsets are not strictly ascending
  deriving (Show, Eq)

-- | Creates an IndexPage from a ByteString, validating the data format
-- Returns Left with an error if the data is invalid
fromByteString :: ByteString -> Either IndexPageError IndexPage
fromByteString rawPage = do
  let pageSize = ByteString.length rawPage
  (InvalidPageSizeError pageSize) `whenNot` (pageSize == 8192)
  validateSegments rawPage 0 (0, 0)
  return $ IndexPage rawPage
  where
    whenNot err cond = if cond then Right () else Left err
    validateSegments bs ix (lastPage, lastEvent) =
      if ix >= 512 then Right ()
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
      if ix >= 512 then Right ()
      else do
        let offset = ix * 16
        let pageOffset = readW64BE bs offset
        let eventOffset = readW64BE bs (offset + 8)
        (NonZeroTrailingSegmentError ix) `whenNot` (pageOffset == 0 && eventOffset == 0)
        validateTrailingZeros bs (ix + 1)

-- | Converts an IndexPage back to its raw ByteString representation
toByteString :: IndexPage -> ByteString
toByteString (IndexPage rawPage) = rawPage

-- | Represents an entry in the index containing page and event offset information
data IndexEntry = IndexEntry {
  pageOffset :: Word64,        -- ^ Offset of the page in the store
  minimumEventOffset :: Word64 -- ^ Minimum event offset contained in the page
} deriving (Show, Eq)

-- | Returns all valid entries in the index page
entries :: IndexPage -> [IndexEntry]
entries page = 
  [ e | ix <- [0..63], 
    let e = entry page ix, 
    pageOffset e /= 0 || minimumEventOffset e /= 0 ]

-- | Returns the entry at the specified index
entry :: IndexPage -> Int -> IndexEntry
entry (IndexPage rawPage) ix =
  let offset = ix * 16
      pOffset = readW64BE rawPage offset
      eOffset = readW64BE rawPage (offset + 8)
   in IndexEntry { pageOffset = pOffset, minimumEventOffset = eOffset }

-- | Returns the number of valid entries in the index page
entryCount :: IndexPage -> Int
entryCount (IndexPage rawPage) = count 0
  where
    count ix = 
      if ix >= 512 then 0
      else let offset = ix * 16
               pageOffset = readW64BE rawPage offset
               eventOffset = readW64BE rawPage (offset + 8)
           in if pageOffset == 0 && eventOffset == 0 
              then 0 
              else 1 + count (ix + 1)