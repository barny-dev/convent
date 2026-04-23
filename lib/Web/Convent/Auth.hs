{-# LANGUAGE OverloadedStrings #-}

module Web.Convent.Auth
  ( AuthStore
  , newAuthStore
  , registerUser
  , exchangePasswordForToken
  , authenticateToken
  ) where

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA256)
import qualified Data.ByteArray.Encoding as ByteArray

data UserCredentials = UserCredentials
  { credentialSalt :: Text
  , credentialPasswordHash :: Text
  }

data AuthStore = AuthStore
  { users :: IORef (Map Text UserCredentials)
  , tokens :: IORef (Map Text (Text, UTCTime))
  }

tokenExpirationSeconds :: NominalDiffTime
tokenExpirationSeconds = 3600 -- one hour

passwordHashParameters :: Parameters
passwordHashParameters = Parameters
  { iterCounts = 100000
  , outputLength = 32
  }

newAuthStore :: IO AuthStore
newAuthStore = do
  users <- newIORef Map.empty
  tokens <- newIORef Map.empty
  pure AuthStore{users, tokens}

registerUser :: AuthStore -> Text -> Text -> IO (Either String ())
registerUser AuthStore{users} username password
  | Text.null (Text.strip username) = pure $ Left "Username cannot be empty"
  | Text.length password < 8 = pure $ Left "Password must be at least 8 characters long"
  | otherwise = do
      salt <- UUID.toText <$> nextRandom
      let passwordHash = hashPassword salt password
      atomicModifyIORef' users $ \current ->
        if Map.member username current
          then (current, Left "User already exists")
          else
            let credentials = UserCredentials salt passwordHash
            in (Map.insert username credentials current, Right ())

exchangePasswordForToken :: AuthStore -> Text -> Text -> IO (Either String Text)
exchangePasswordForToken AuthStore{users, tokens} username password = do
  now <- getCurrentTime
  authResult <- atomicModifyIORef' users $ \current ->
    case Map.lookup username current of
      Nothing -> (current, Left "Invalid username or password")
      Just (UserCredentials salt storedHash)
        | storedHash == hashPassword salt password -> (current, Right ())
        | otherwise -> (current, Left "Invalid username or password")
  case authResult of
    Left err -> pure $ Left err
    Right () -> do
      token <- UUID.toText <$> nextRandom
      let expiresAt = addUTCTime tokenExpirationSeconds now
      atomicModifyIORef' tokens $ \current ->
        (Map.insert token (username, expiresAt) current, ())
      pure $ Right token

authenticateToken :: AuthStore -> Text -> IO (Maybe Text)
authenticateToken AuthStore{tokens} token = do
  now <- getCurrentTime
  atomicModifyIORef' tokens $ \current ->
    let activeTokens = Map.filter (\(_, expiry) -> expiry > now) current
    in (activeTokens, fmap fst (Map.lookup token activeTokens))

hashPassword :: Text -> Text -> Text
hashPassword salt password =
  let encodedPassword = TextEncoding.encodeUtf8 password
      encodedSalt = TextEncoding.encodeUtf8 salt
      derivedKey :: ByteString
      derivedKey = fastPBKDF2_SHA256 passwordHashParameters encodedPassword encodedSalt
  in TextEncoding.decodeUtf8 (ByteArray.convertToBase ByteArray.Base16 derivedKey)
