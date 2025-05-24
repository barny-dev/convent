module Web.Convent.Storage.IndexPage
  ( IndexPage()
  , IndexPageError(..)
  , fromByteString
  , toByteString
  , entryCount
  , emptyPage
  , IndexEntry(..)
  , entries
  , entry
  , addEntry
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word64)
import Web.Convent.Util.ByteString (readW64BE, writeW64BE)

newtype IndexPage = IndexPage ByteString deriving (Eq)

instance Show IndexPage where
  show page = "IndexPage { segments: " ++ show (entryCount page) ++ " }"

data IndexPageError =
  InvalidPageSizeError Int |      -- ^ Page size is not 8192 bytes
  InvalidEntryError Int |         -- ^ Entry data is invalid
  NonZeroTrailingEntryError Int | -- ^ Found non-zero entry after a zero entry
  NonAscendingEventOffsetError Int -- ^ Event offsets are not strictly ascending
  deriving (Show, Eq)

fromByteString :: ByteString -> Either IndexPageError IndexPage
fromByteString rawPage = do
  let pageSize = ByteString.length rawPage
  (InvalidPageSizeError pageSize) `whenNot` (pageSize == 8192)
  validateEntries rawPage 0 0
  return $ IndexPage rawPage
  where
    whenNot err cond = if cond then Right () else Left err
    validateEntries bs ix lastEvent =
      if ix >= 1024 then Right ()
      else do
        let offset = ix * 8
        let eventOffset = readW64BE bs offset
        if eventOffset == 0
          then validateTrailingZeros bs (ix + 1)
          else do
            (NonAscendingEventOffsetError ix) `whenNot` (eventOffset > lastEvent)
            validateEntries bs (ix + 1) eventOffset
    validateTrailingZeros bs ix =
      if ix >= 1024 then Right ()
      else do
        let offset = ix * 8
        let eventOffset = readW64BE bs offset
        (NonZeroTrailingEntryError ix) `whenNot` (eventOffset == 0)
        validateTrailingZeros bs (ix + 1)

emptyPage :: IndexPage
emptyPage = IndexPage $ ByteString.replicate 8192 0

toByteString :: IndexPage -> ByteString
toByteString (IndexPage rawPage) = rawPage

newtype IndexEntry = IndexEntry {
  minimumEventOffset :: Word64 -- ^ Minimum event offset contained in the page
} deriving (Show, Eq)

entries :: IndexPage -> [IndexEntry]
entries page = 
  [ e | ix <- [0..1023], 
    let e = entry page ix, 
    minimumEventOffset e /= 0 ]

entry :: IndexPage -> Int -> IndexEntry
entry (IndexPage rawPage) ix =
  let offset = ix * 8
      eOffset = readW64BE rawPage offset
   in IndexEntry { minimumEventOffset = eOffset }

-- | Adds a new entry to the index page
-- 
-- @param page The index page to add the entry to
-- @param eventOffset The minimum event offset for the new entry
-- @returns Nothing if the page is full or the offset would violate ordering, Just newPage otherwise
--
-- The function ensures:
-- * Event offsets remain strictly ascending
-- * No gaps between valid entries
-- * All entries after first zero entry remain zero
data AddEntryError = 
    PageFull           -- ^ No more space in index page
  | NonAscendingOffset -- ^ New offset not higher than previous entry
  deriving (Show, Eq)

addEntry :: IndexPage -> Word64 -> Either AddEntryError IndexPage
addEntry page@(IndexPage rawPage) newOffset = 
  let count = entryCount page
      lastOffset = if count == 0 then 0 else minimumEventOffset (entry page (count - 1))
   in if count >= 1024 
      then Left PageFull
      else if count > 0 && newOffset <= lastOffset
      then Left NonAscendingOffset
      else Right . IndexPage . ByteString.concat $ [
        ByteString.take (count * 8) rawPage,
        writeW64BE newOffset,
        ByteString.replicate (8192 - (count + 1) * 8) 0
      ]

entryCount :: IndexPage -> Int
entryCount (IndexPage rawPage) = count 0
  where
    count ix = 
      if ix >= 1024 then 0
      else let offset = ix * 8
               eventOffset = readW64BE rawPage offset
           in if eventOffset == 0 
              then 0 
              else 1 + count (ix + 1)