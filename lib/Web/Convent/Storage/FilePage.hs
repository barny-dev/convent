{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Trans.Except (ExceptT (..), except, withExceptT, throwE, catchE, runExceptT)

newtype Index = Index Int deriving (Show, Eq)
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
read :: IO.Handle -> Ptr -> IO (Either ReadError ByteString)
read fh (Index ix, Size ps) = (handle (\err -> ReadIOError err) read') >>= evaluate
  where read' = runExceptT $! do 
                if po < 0 
                  then throwE ReadInvalidPageIndex ix
                  else return ()
                fileSize <- lift IO.hFileSize hf
                let offset = fromIntegral ix * fromIntegral ps
                    requiredSize = offset + fromIntegral ps
                if fileSize < requiredSize
                  then throwE $ ReadFileTooSmall (fromIntegral requiredSize) (fromIntegral fileSize)
                  else return ()
                lift $ IO.hSeek fh IO.AbsoluteSeek offset
                pageData <- lift $ ByteString.hGet fh (fromIntegral ps)
                return Right $! pageData

data WriteError = 
   WriteInvalidPageIndex Index | 
   WriteFileTooSmall Integer Integer |
   WritePageSizeMismatch Size Size |
   WriteIOError Prelude.IOError deriving (Show, Eq)

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
write :: IO.Handle -> Ptr -> ByteString -> IO (Either WriteError ())
write fh (Index ix, Size ps) pageData = (handle (\err -> WriteIOError err) write') >>= evaluate
  where write' = runExceptT $! do
                 if ix < 0 
                   then throwE WriteInvalidPageOffset ix
                   else return ()
                 if fromIntegral ps /= ByteString.length pageData 
                   then throw WritePageSizeMismatch ps (fromIntegral $ ByteString.length pageData) 
                   else return ()
                 let offset = fromIntegral ix * fromIntegral ps
                 lift $ IO.hSeek fh IO.AbsoluteSeek offset
                 lift $ ByteString.hPut fh pageData
          
class FilePage a where
  type FilePageLoadError a :: *
  type FilePageSaveError a :: *
  toByteString :: a -> Either (FilePageSaveError a) ByteString
  fromByteString :: ByteString -> Either (FilePageLoadError a) a
  mapWriteError :: WriteError -> FilePageSaveError a
  mapReadError :: ReadError -> FilePageLoadError a

loadPage :: FilePage a => IO.Handle -> Ptr -> IO (Either (FilePageLoadError a) a)
loadPage fh ptr = do
    result <- read fh ptr
    return $! case result of
        Left err -> Left $! mapReadError err
        Right byteData -> fromByteString $! byteData