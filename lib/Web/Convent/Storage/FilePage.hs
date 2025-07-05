{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, GeneralizedNewtypeDeriving #-}
-- | Low-level file page I/O operations for fixed-size page storage.
--
-- This module provides utilities for reading and writing fixed-size pages
-- to file handles, with proper error handling and validation.
module Web.Convent.Storage.FilePage
  ( read
  , write
  , ReadError(..)
  , WriteError(..)
  , FilePage(..)
  , Index(..)
  , Size(..)
  ) where

import Prelude hiding (read, IOError)
import qualified Prelude as Prelude (IOError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified System.IO as IO
import Control.Exception (handle, evaluate)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, withExceptT, throwE, catchE, runExceptT)
import Data.Kind (Type)

-- | Zero-based page index within a file.
newtype Index = Index Int deriving (Show, Eq)

-- | Size of a page in bytes.
newtype Size = Size Int deriving (Show, Eq)
type Ptr = (Index, Size)

-- | Errors that can occur during page read operations.
data ReadError =
    ReadFileTooSmall Int Int  -- ^ File too small: required size, actual size
  | ReadInvalidPageIndex Index  -- ^ Invalid page index (negative)
  | ReadIOError Prelude.IOError  -- ^ Low-level I/O error
  deriving (Show, Eq)

-- | Read a page from a file handle at the specified index and size.
--
-- The page is read from the file at offset @index * pageSize@.
-- Returns the page data as a 'ByteString' or a 'ReadError' if the operation fails.
--
-- Validates that:
-- * The page index is non-negative
-- * The file is large enough to contain the requested page
read :: IO.Handle -> Ptr -> IO (Either ReadError ByteString)
read fh (Index ix, Size ps) = (wrapIOError (\err -> ReadIOError err) read') >>= evaluate
  where read' = runExceptT $! do 
                if ix < 0 
                  then throwE $ ReadInvalidPageIndex (Index ix)
                  else return ()
                fileSize <- lift $ IO.hFileSize fh
                let offset = fromIntegral ix * fromIntegral ps
                    requiredSize = offset + fromIntegral ps
                if fileSize < requiredSize
                  then throwE $ ReadFileTooSmall (fromIntegral requiredSize) (fromIntegral fileSize)
                  else return ()
                lift $ IO.hSeek fh IO.AbsoluteSeek offset
                pageData <- lift $ ByteString.hGet fh (fromIntegral ps)
                return $! pageData

-- | Errors that can occur during page write operations.
data WriteError = 
   WriteInvalidPageIndex Index  -- ^ Invalid page index (negative)
   | WriteFileTooSmall Integer Integer  -- ^ File too small: required size, actual size
   | WritePageSizeMismatch Size Size  -- ^ Page size mismatch: expected size, actual data size
   | WriteIOError Prelude.IOError  -- ^ Low-level I/O error
   deriving (Show, Eq)

-- | Write a page to a file handle at the specified index and size.
--
-- The page is written to the file at offset @index * pageSize@.
-- Returns @()@ on success or a 'WriteError' if the operation fails.
--
-- Validates that:
-- * The page index is non-negative
-- * The data size matches the expected page size
write :: IO.Handle -> Ptr -> ByteString -> IO (Either WriteError ())
write fh (Index ix, Size ps) pageData = (wrapIOError (\err -> WriteIOError err) write') >>= evaluate
  where write' = runExceptT $! do
                 if ix < 0 
                   then throwE $ WriteInvalidPageIndex (Index ix)
                   else return ()
                 if fromIntegral ps /= ByteString.length pageData 
                   then throwE $ WritePageSizeMismatch (Size ps) (Size . fromIntegral $ ByteString.length pageData) 
                   else return ()
                 let offset = fromIntegral ix * fromIntegral ps
                 lift $ IO.hSeek fh IO.AbsoluteSeek offset
                 lift $ ByteString.hPut fh pageData
          
-- | Type class for data structures that can be stored as file pages.
--
-- Instances of this class can be serialized to and from 'ByteString' format
-- for storage in fixed-size pages, with proper error handling and mapping.
class FilePage a where
  -- | Errors that can occur when loading a page from bytes.
  data FilePageLoadError a :: Type
  
  -- | Errors that can occur when saving a page to bytes.
  data FilePageSaveError a :: Type
  
  -- | Convert a page to its byte representation.
  toByteString :: a -> Either (FilePageSaveError a) ByteString
  
  -- | Parse a page from its byte representation.
  fromByteString :: ByteString -> Either (FilePageLoadError a) a
  
  -- | Map low-level write errors to page-specific save errors.
  mapWriteError :: WriteError -> FilePageSaveError a
  
  -- | Map low-level read errors to page-specific load errors.
  mapReadError :: ReadError -> FilePageLoadError a

-- | Load a typed page from a file handle.
--
-- This is a convenience function that combines 'read' with 'fromByteString'
-- to load a strongly-typed page from storage.
loadPage :: FilePage a => IO.Handle -> Ptr -> IO (Either (FilePageLoadError a) a)
loadPage fh ptr = do
    result <- read fh ptr
    return $! case result of
        Left err -> Left $! mapReadError err
        Right byteData -> fromByteString $! byteData

wrapIOError :: (Prelude.IOError -> e) -> IO (Either e a) -> IO (Either e a)
wrapIOError f io = handle (\err -> return $ Left $! f err) $! io
