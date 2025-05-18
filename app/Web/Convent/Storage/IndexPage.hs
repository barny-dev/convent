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
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word64)
import Web.Convent.Util.ByteString (readW64BE)

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