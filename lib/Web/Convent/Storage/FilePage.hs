{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, GeneralizedNewtypeDeriving #-}
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

newtype Index = Index Int deriving (Show, Eq)
newtype Size = Size Int deriving (Show, Eq)
type Ptr = (Index, Size)

data ReadError =
    ReadFileTooSmall Int Int
  | ReadInvalidPageIndex Index
  | ReadIOError Prelude.IOError
  deriving (Show, Eq)

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

data WriteError = 
   WriteInvalidPageIndex Index | 
   WriteFileTooSmall Integer Integer |
   WritePageSizeMismatch Size Size |
   WriteIOError Prelude.IOError deriving (Show, Eq)

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
          
class FilePage a where
  data FilePageLoadError a :: Type
  data FilePageSaveError a :: Type
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

wrapIOError :: (Prelude.IOError -> e) -> IO (Either e a) -> IO (Either e a)
wrapIOError f io = handle (\err -> return $ Left $! f err) $! io
