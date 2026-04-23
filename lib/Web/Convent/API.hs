{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Convent.API
  ( API
  , server
  , AuthHeader
  , ChatId(..)
  , RegisterRequest(..)
  , ExchangePasswordRequest(..)
  , ExchangePasswordResponse(..)
  , CreateChatResponse(..)
  , JoinChatRequest(..)
  , JoinChatResponse(..)
  , PostMessageRequest(..)
  , PostMessageResponse(..)
  , GetEventsResponse(..)
  , EventResponse(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Word (Word64)
import GHC.Generics (Generic)
import Servant

import Web.Convent.Auth (AuthStore)
import qualified Web.Convent.Auth as Auth
import Web.Convent.Storage.ChatStore (ChatStore)
import qualified Web.Convent.Storage.ChatStore as ChatStore
import Web.Convent.Storage.ChatDataOps (EventResponse(..))

-- | Chat identifier
newtype ChatId = ChatId UUID
  deriving (Eq, Show, Generic)

instance ToJSON ChatId where
  toJSON (ChatId uuid) = toJSON (UUID.toText uuid)

instance FromJSON ChatId where
  parseJSON v = do
    txt <- parseJSON v
    case UUID.fromText txt of
      Just uuid -> return (ChatId uuid)
      Nothing -> fail "Invalid UUID format"

instance ToHttpApiData ChatId where
  toUrlPiece (ChatId uuid) = UUID.toText uuid

instance FromHttpApiData ChatId where
  parseUrlPiece txt = case UUID.fromText txt of
    Just uuid -> Right (ChatId uuid)
    Nothing -> Left "Invalid UUID format"

-- | Response from creating a chat
data CreateChatResponse = CreateChatResponse
  { chatId :: ChatId
  } deriving (Show, Generic)

instance ToJSON CreateChatResponse
instance FromJSON CreateChatResponse

data RegisterRequest = RegisterRequest
  { username :: Text
  , password :: Text
  } deriving (Show, Generic)

instance ToJSON RegisterRequest
instance FromJSON RegisterRequest

data ExchangePasswordRequest = ExchangePasswordRequest
  { exchangeUsername :: Text
  , exchangePassword :: Text
  } deriving (Show, Generic)

instance ToJSON ExchangePasswordRequest where
  toJSON (ExchangePasswordRequest u p) =
    object ["username" .= u, "password" .= p]

instance FromJSON ExchangePasswordRequest where
  parseJSON = withObject "ExchangePasswordRequest" $ \v ->
    ExchangePasswordRequest <$> v .: "username" <*> v .: "password"

newtype ExchangePasswordResponse = ExchangePasswordResponse
  { token :: Text
  } deriving (Show, Generic)

instance ToJSON ExchangePasswordResponse
instance FromJSON ExchangePasswordResponse

-- | Request to join a chat
data JoinChatRequest = JoinChatRequest
  { participantName :: Text
  } deriving (Show, Generic)

instance ToJSON JoinChatRequest
instance FromJSON JoinChatRequest

-- | Response from joining a chat
data JoinChatResponse = JoinChatResponse
  { joinParticipantId :: Word64
  , joinEventOffset :: Word64
  } deriving (Show, Generic)

instance ToJSON JoinChatResponse where
  toJSON (JoinChatResponse pid offset) = 
    object ["participantId" .= pid, "eventOffset" .= offset]
instance FromJSON JoinChatResponse where
  parseJSON = withObject "JoinChatResponse" $ \v ->
    JoinChatResponse <$> v .: "participantId" <*> v .: "eventOffset"

-- | Request to post a message
data PostMessageRequest = PostMessageRequest
  { messageParticipantId :: Word64
  , messageText :: Text
  } deriving (Show, Generic)

instance ToJSON PostMessageRequest
instance FromJSON PostMessageRequest

-- | Response from posting a message
data PostMessageResponse = PostMessageResponse
  { messageEventOffset :: Word64
  } deriving (Show, Generic)

instance ToJSON PostMessageResponse
instance FromJSON PostMessageResponse

-- | Response from getting events (re-export EventResponse from ChatStore)
data GetEventsResponse = GetEventsResponse
  { events :: [EventResponse]
  } deriving (Show, Generic)

instance ToJSON GetEventsResponse where
  toJSON (GetEventsResponse evts) = 
    object ["events" .= map eventToJSON evts]
    where
      eventToJSON (EventResponse offset typ dat) =
        object ["eventOffset" .= offset, "eventType" .= typ, "eventData" .= dat]

instance FromJSON GetEventsResponse where
  parseJSON = withObject "GetEventsResponse" $ \v -> do
    evts <- v .: "events"
    return $ GetEventsResponse evts

-- | API definition
type API = 
       "users" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] NoContent
  :<|> "auth" :> "token" :> ReqBody '[JSON] ExchangePasswordRequest :> Post '[JSON] ExchangePasswordResponse
  :<|> AuthHeader :> "chats" :> Post '[JSON] CreateChatResponse
  :<|> AuthHeader :> "chats" :> Capture "id" ChatId :> "join" :> ReqBody '[JSON] JoinChatRequest :> Post '[JSON] JoinChatResponse
  :<|> AuthHeader :> "chats" :> Capture "id" ChatId :> "messages" :> ReqBody '[JSON] PostMessageRequest :> Post '[JSON] PostMessageResponse
  :<|> AuthHeader :> "chats" :> Capture "id" ChatId :> "events" :> QueryParam "offset" Word64 :> Get '[JSON] GetEventsResponse

type AuthHeader = Header' '[Required, Strict] "Authorization" Text

-- | Server implementation
server :: AuthStore -> ChatStore -> Server API
server authStore store = registerUser authStore
                   :<|> exchangePasswordForToken authStore
                   :<|> createChat authStore store
                   :<|> joinChat authStore store
                   :<|> postMessage authStore store
                   :<|> getEvents authStore store

-- | Create a new chat
createChat :: AuthStore -> ChatStore -> Text -> Handler CreateChatResponse
createChat authStore store authorization = do
  _ <- requireAuthenticatedUser authStore authorization
  -- Call ChatStore to create a new chat
  uuid <- liftIO $ ChatStore.createChat store
  return $ CreateChatResponse (ChatId uuid)

registerUser :: AuthStore -> RegisterRequest -> Handler NoContent
registerUser authStore RegisterRequest{..} = do
  result <- liftIO $ Auth.registerUser authStore username password
  case result of
    Left "User already exists" ->
      throwError err409 { errBody = "User already exists" }
    Left err ->
      throwError err400 { errBody = TLE.encodeUtf8 $ TL.pack err }
    Right () -> pure NoContent

exchangePasswordForToken :: AuthStore -> ExchangePasswordRequest -> Handler ExchangePasswordResponse
exchangePasswordForToken authStore ExchangePasswordRequest{..} = do
  result <- liftIO $ Auth.exchangePasswordForToken authStore exchangeUsername exchangePassword
  case result of
    Left _ -> throwError err401 { errBody = "Invalid username or password" }
    Right tok -> pure $ ExchangePasswordResponse tok

-- | Join a chat
joinChat :: AuthStore -> ChatStore -> Text -> ChatId -> JoinChatRequest -> Handler JoinChatResponse
joinChat authStore store authorization (ChatId uuid) JoinChatRequest{..} = do
  _ <- requireAuthenticatedUser authStore authorization
  -- Check if chat exists
  exists <- liftIO $ ChatStore.chatExists store uuid
  if not exists
    then throwError err404 { errBody = "Chat not found" }
    else do
      -- Join the chat via ChatStore
      result <- liftIO $ ChatStore.joinChatParticipant store uuid participantName
      case result of
        Left err -> throwError err500 { errBody = TLE.encodeUtf8 $ TL.pack $ "Failed to join chat: " ++ err }
        Right (pid, eventOffset) -> return $ JoinChatResponse pid eventOffset

-- | Post a message to a chat
postMessage :: AuthStore -> ChatStore -> Text -> ChatId -> PostMessageRequest -> Handler PostMessageResponse
postMessage authStore store authorization (ChatId uuid) PostMessageRequest{..} = do
  _ <- requireAuthenticatedUser authStore authorization
  -- Check if chat exists
  exists <- liftIO $ ChatStore.chatExists store uuid
  if not exists
    then throwError err404 { errBody = "Chat not found" }
    else do
      -- Post message via ChatStore
      result <- liftIO $ ChatStore.postChatMessage store uuid messageParticipantId messageText
      case result of
        Left err -> throwError err500 { errBody = TLE.encodeUtf8 $ TL.pack $ "Failed to post message: " ++ err }
        Right eventOffset -> return $ PostMessageResponse eventOffset

-- | Get events from a chat starting from an offset
getEvents :: AuthStore -> ChatStore -> Text -> ChatId -> Maybe Word64 -> Handler GetEventsResponse
getEvents authStore store authorization (ChatId uuid) maybeOffset = do
  _ <- requireAuthenticatedUser authStore authorization
  let offset = maybe 0 id maybeOffset
  
  -- Check if chat exists
  exists <- liftIO $ ChatStore.chatExists store uuid
  if not exists
    then throwError err404 { errBody = "Chat not found" }
    else do
      -- Get events via ChatStore
      result <- liftIO $ ChatStore.getChatEvents store uuid offset
      case result of
        Left err -> throwError err500 { errBody = TLE.encodeUtf8 $ TL.pack $ "Failed to get events: " ++ err }
        Right eventsData -> return $ GetEventsResponse eventsData

requireAuthenticatedUser :: AuthStore -> Text -> Handler Text
requireAuthenticatedUser authStore authorization =
  case Text.stripPrefix "Bearer " authorization of
    Nothing -> throwError err401 { errBody = "Authorization header must use Bearer token" }
    Just tokenValue -> do
      maybeUser <- liftIO $ Auth.authenticateToken authStore tokenValue
      case maybeUser of
        Nothing -> throwError err401 { errBody = "Invalid token" }
        Just user -> pure user
