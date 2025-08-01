{-# LANGUAGE FunctionalDependencies #-}
module Web.Convent.Storage.PageOps
    ( Index
    , PageOps
    , PageOpsM
    , FilePageOps ()
    ) where
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Control.Exception (Exception (displayException)) 
import Control.Exception qualified as Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.State qualified as State
import Data.Kind (Type)
import System.IO qualified as IO
import Web.Convent.Error (Error (..))
import Data.Functor ((<&>))  

newtype Index = Index Int deriving (Show, Eq)

type PageOpsM s m = ExceptT Error (StateT s m)

class Monad m => PageOps s m | s -> m where
    readPage :: Index ->  PageOpsM s m ByteString
    writePage :: Index -> ByteString -> PageOpsM s m ()

data FilePageOps = FilePageOps IO.Handle Int

filePageOps :: IO.Handle -> Int -> FilePageOps
filePageOps = FilePageOps

instance PageOps FilePageOps IO where
    readPage (Index ix) = do
        FilePageOps fileHandle pageSize <- lift State.get
        let offset = fromIntegral $ ix * pageSize
        ExceptT . lift . wrapError $ IO.hSeek fileHandle IO.AbsoluteSeek offset
        ExceptT . lift . wrapError $ ByteString.hGet fileHandle (fromIntegral pageSize)
    writePage (Index ix) pageData = do
        FilePageOps fileHandle pageSize <- lift State.get
        let offset = fromIntegral $ ix * pageSize
        ExceptT . lift . wrapError $ IO.hSeek fileHandle IO.AbsoluteSeek offset
        ExceptT . lift . wrapError $ ByteString.hPut fileHandle pageData
        return ()

wrapError :: IO a -> IO (Either Error a)
wrapError action = Exception.handle 
    (\(e :: IOError) -> return $ Left (convertError e))
    (action >>= \a -> return $! Right $! a)

convertError :: IOError -> Error
convertError e = Error { errorMessage = displayException e, errorCause = Nothing}