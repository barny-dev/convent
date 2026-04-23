{-# LANGUAGE OverloadedStrings #-}

module Web.Convent.Auth
  ( AuthStore
  , newAuthStore
  , registerUser
  , exchangePasswordForToken
  , authenticateToken
  ) where

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

data AuthStore = AuthStore
  { users :: IORef (Map Text Text)
  , tokens :: IORef (Map Text Text)
  }

newAuthStore :: IO AuthStore
newAuthStore = do
  users <- newIORef Map.empty
  tokens <- newIORef Map.empty
  pure AuthStore{users, tokens}

registerUser :: AuthStore -> Text -> Text -> IO (Either String ())
registerUser AuthStore{users} username password
  | Text.null (Text.strip username) = pure $ Left "Username cannot be empty"
  | Text.null password = pure $ Left "Password cannot be empty"
  | otherwise = pure $ atomicModifyIORef' users $ \current ->
      if Map.member username current
        then (current, Left "User already exists")
        else (Map.insert username password current, Right ())

exchangePasswordForToken :: AuthStore -> Text -> Text -> IO (Either String Text)
exchangePasswordForToken AuthStore{users, tokens} username password = do
  authResult <- atomicModifyIORef' users $ \current ->
    case Map.lookup username current of
      Nothing -> (current, Left "Invalid username or password")
      Just storedPassword
        | storedPassword == password -> (current, Right ())
        | otherwise -> (current, Left "Invalid username or password")
  case authResult of
    Left err -> pure $ Left err
    Right () -> do
      token <- UUID.toText <$> nextRandom
      atomicModifyIORef' tokens $ \current ->
        (Map.insert token username current, ())
      pure $ Right token

authenticateToken :: AuthStore -> Text -> IO (Maybe Text)
authenticateToken AuthStore{tokens} token =
  atomicModifyIORef' tokens $ \current ->
    (current, Map.lookup token current)
