{-# LANGUAGE TypeFamilies #-}
module Web.Convent.Storage.FilePage
  ( read
  , write
  , FileReadError(..)
  ) where

import Prelude hiding (read, IOError)
import qualified Prelude as Prelude (IOError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified System.IO as IO
import Control.Exception (handle, evaluate)

newtype Index = Offset Int deriving (Show, Eq)
newtype Size = Size Int deriving (Show, Eq)
type Ptr = (Index, Size)
-- | Errors that can occur when reading pages from a file
data ReadError =
    ReadFileTooSmall Int Int                 -- ^ File is too small (expected size, actual size)
  | ReadInvalidPageOffset Int                -- ^ Tried to read from invalid page offset
  | ReadIOError Prelude.IOError              -- ^ Underlying IO operation failed
  deriving (Show, Eq)

-- | Reads a page from a file at the specified index.
-- 
-- @param handle The file handle to read from
-- @param pageIdx The zero-based page index to read
-- @param pageSize The size of each page in bytes
-- @returns Either an error describing why reading failed, or the raw page data
--
-- Returns an error if the file is too small or if the page index is invalid.
read :: IO.Handle -> FilePagePtr -> IO (Either ReadError ByteString)
read fh ((Index ix), (Size sz)) = (handle (\err -> ReadIOError err) read') >>= evaluate
  where read' = do
    if pageIdx < 0 
      then return $ Left $ InvalidPageOffset pageIdx
      else do
        fileSize <- IO.hFileSize hf
        let offset = fromIntegral pageIdx * fromIntegral pageSize
        let requiredSize = offset + fromIntegral pageSize
        if fileSize < requiredSize
          then return $ Left $ FileTooSmall (fromIntegral requiredSize) (fromIntegral fileSize)
          else do
            IO.hSeek fh IO.AbsoluteSeek offset
            pageData <- ByteString.hGet fh pageSize
            return Right $! pageData

data WriteError = WriteIOError Prelude.IOError deriving (Show, Eq)

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
write :: IO.Handle -> FilePagePtr -> ByteString -> IO (Either WriteError ())
write fh pageIdx pageSize pageData = (handle (\err -> IOError err) write') >>= evaluate
  where write' = do
    let offset = fromIntegral pageIdx * fromIntegral pageSize
    IO.hSeek fh IO.AbsoluteSeek offset
    ByteString.hPut fh pageData

class FilePage a where
  type FilePageLoadError a :: *
  type FilePageSaveError
  toByteString :: a -> Either 