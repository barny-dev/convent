
module Web.Convent.Storage.FilePage
  ( read
  , write
  , FileReadError(..)
  ) where

import Prelude hiding (read)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified System.IO as IO

-- | Errors that can occur when reading pages from a file
data FileReadError =
    FileTooSmall Int Int                 -- ^ File is too small (expected size, actual size)
  | InvalidPageOffset Int                -- ^ Tried to read from invalid page offset
  | IOError IOError                      -- ^ Underlying IO operation failed
  deriving (Show, Eq)

-- | Reads a page from a file at the specified index.
-- 
-- @param handle The file handle to read from
-- @param pageIdx The zero-based page index to read
-- @param pageSize The size of each page in bytes
-- @returns Either an error describing why reading failed, or the raw page data
--
-- Returns an error if the file is too small or if the page index is invalid.
read :: IO.Handle -> Int -> Int -> IO (Either FileReadError ByteString)
read handle pageIdx pageSize = do
  if pageIdx < 0 
    then return $ Left $ InvalidPageOffset pageIdx
    else do
      fileSize <- IO.hFileSize handle
      let offset = fromIntegral pageIdx * fromIntegral pageSize
      let requiredSize = offset + fromIntegral pageSize
      if fileSize < requiredSize
        then return $ Left $ FileTooSmall (fromIntegral requiredSize) (fromIntegral fileSize)
        else do
          IO.hSeek handle IO.AbsoluteSeek offset
          pageData <- ByteString.hGet handle pageSize
          return $ Right pageData

-- | Writes a page to a file at the specified index.
--
-- @param handle The file handle to write to
-- @param pageIdx The zero-based page index to write
-- @param pageSize The size of each page in bytes
-- @param pageData The raw page data to write
-- @returns IO action that performs the write
--
-- Will overwrite any existing data at that position. The pageData should
-- be exactly pageSize bytes, but this is not enforced by this function.
write :: IO.Handle -> Int -> Int -> ByteString -> IO ()
write handle pageIdx pageSize pageData = do
  let offset = fromIntegral pageIdx * fromIntegral pageSize
  IO.hSeek handle IO.AbsoluteSeek offset
  ByteString.hPut handle pageData
