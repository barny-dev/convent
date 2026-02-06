{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Convent.API
  ( API
  , server
  , ChatId(..)
  , CreateChatResponse(..)
  , JoinChatRequest(..)
  , JoinChatResponse(..)
  , PostMessageRequest(..)
  , PostMessageResponse(..)
  , GetEventsResponse(..)
  , EventResponse(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Data.Word (Word64)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Servant
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)

import Web.Convent.Storage.ChatStore (ChatStore, ChatData(..))
import qualified Web.Convent.Storage.ChatStore as ChatStore
import Web.Convent.Storage.EventsPage (EventsPage)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import Web.Convent.Storage.IndexPage (IndexPage)
import qualified Web.Convent.Storage.IndexPage as IndexPage
import qualified Web.Convent.Storage.FilePage as FilePage
import qualified Web.Convent.Events.ParticipantJoinedEvent as ParticipantJoinedEvent
import qualified Web.Convent.Events.MessageSubmittedEvent as MessageSubmittedEvent

-- | Chat identifier
newtype ChatId = ChatId UUID
  deriving (Eq, Show, Generic)

instance ToJSON ChatId
instance FromJSON ChatId

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

-- | Individual event in response
data EventResponse = EventResponse
  { responseEventOffset :: Word64
  , responseEventType :: Word64
  , responseEventData :: Text
  } deriving (Show, Generic)

instance ToJSON EventResponse where
  toJSON (EventResponse offset typ dat) =
    object ["eventOffset" .= offset, "eventType" .= typ, "eventData" .= dat]
instance FromJSON EventResponse where
  parseJSON = withObject "EventResponse" $ \v ->
    EventResponse <$> v .: "eventOffset" <*> v .: "eventType" <*> v .: "eventData"

-- | Response from getting events
data GetEventsResponse = GetEventsResponse
  { events :: [EventResponse]
  } deriving (Show, Generic)

instance ToJSON GetEventsResponse
instance FromJSON GetEventsResponse

-- | API definition
type API = 
       "chats" :> Post '[JSON] CreateChatResponse
  :<|> "chats" :> Capture "id" ChatId :> "join" :> ReqBody '[JSON] JoinChatRequest :> Post '[JSON] JoinChatResponse
  :<|> "chats" :> Capture "id" ChatId :> "messages" :> ReqBody '[JSON] PostMessageRequest :> Post '[JSON] PostMessageResponse
  :<|> "chats" :> Capture "id" ChatId :> "events" :> QueryParam "offset" Word64 :> Get '[JSON] GetEventsResponse

-- | Server implementation
server :: ChatStore -> Server API
server store = createChat store
          :<|> joinChat store
          :<|> postMessage store
          :<|> getEvents store

-- | Create a new chat
createChat :: ChatStore -> Handler CreateChatResponse
createChat _store = do
  -- Generate a new UUID for the chat
  uuid <- liftIO UUID.V4.nextRandom
  let chatIdValue = ChatId uuid
  
  -- Create chat directory
  let chatDir = "chats/" ++ UUID.toString uuid
  liftIO $ createDirectoryIfMissing True chatDir
  
  -- Create empty index and events files
  let indexPath = chatDir ++ "/index.dat"
      eventsPath = chatDir ++ "/events.dat"
  
  -- Create files with initial empty page
  liftIO $ do
    BS.writeFile indexPath (IndexPage.toByteString IndexPage.emptyPage)
    BS.writeFile eventsPath (EventsPage.toByteString EventsPage.emptyPage)
  
  return $ CreateChatResponse chatIdValue

-- | Join a chat
joinChat :: ChatStore -> ChatId -> JoinChatRequest -> Handler JoinChatResponse
joinChat _store (ChatId uuid) JoinChatRequest{..} = do
  let chatDir = "chats/" ++ UUID.toString uuid
  
  -- Check if chat exists
  exists <- liftIO $ doesDirectoryExist chatDir
  unless exists $ throwError err404 { errBody = "Chat not found" }
  
  -- Generate participant ID (simple: use timestamp as ID)
  timestamp <- liftIO getCurrentTimestamp
  let pid = timestamp
  
  let eventsPath = chatDir ++ "/events.dat"
  
  -- Calculate next event offset
  eventOffset <- liftIO $ calculateNextOffset eventsPath
  
  -- Create and append ParticipantJoinedEvent
  let event = ParticipantJoinedEvent.Event
        { ParticipantJoinedEvent.participantId = pid
        , ParticipantJoinedEvent.timestamp = timestamp
        , ParticipantJoinedEvent.participantName = participantName
        }
  
  liftIO $ appendEventToFile eventsPath (ParticipantJoinedEvent.encode event)
  
  return $ JoinChatResponse pid eventOffset

-- | Post a message to a chat
postMessage :: ChatStore -> ChatId -> PostMessageRequest -> Handler PostMessageResponse
postMessage _store (ChatId uuid) PostMessageRequest{..} = do
  let chatDir = "chats/" ++ UUID.toString uuid
  
  -- Check if chat exists
  exists <- liftIO $ doesDirectoryExist chatDir
  unless exists $ throwError err404 { errBody = "Chat not found" }
  
  -- Get current timestamp
  timestamp <- liftIO getCurrentTimestamp
  
  let eventsPath = chatDir ++ "/events.dat"
  
  -- Calculate next event offset
  eventOffsetValue <- liftIO $ calculateNextOffset eventsPath
  
  -- Create and append MessageSubmittedEvent
  let event = MessageSubmittedEvent.Event
        { MessageSubmittedEvent.participantId = messageParticipantId
        , MessageSubmittedEvent.timestamp = timestamp
        , MessageSubmittedEvent.message = messageText
        }
  
  liftIO $ appendEventToFile eventsPath (MessageSubmittedEvent.encode event)
  
  return $ PostMessageResponse eventOffsetValue

-- | Get events from a chat starting from an offset
getEvents :: ChatStore -> ChatId -> Maybe Word64 -> Handler GetEventsResponse
getEvents _store (ChatId uuid) maybeOffset = do
  let chatDir = "chats/" ++ UUID.toString uuid
      offset = maybe 0 id maybeOffset
  
  -- Check if chat exists
  exists <- liftIO $ doesDirectoryExist chatDir
  unless exists $ throwError err404 { errBody = "Chat not found" }
  
  -- Read events from file
  let eventsPath = chatDir ++ "/events.dat"
  eventsData <- liftIO $ readEventsFromFile eventsPath offset
  
  return $ GetEventsResponse eventsData

-- Helper functions

getCurrentTimestamp :: IO Word64
getCurrentTimestamp = do
  posixTime <- getPOSIXTime
  return $ round posixTime

calculateNextOffset :: FilePath -> IO Word64
calculateNextOffset path = do
  content <- BS.readFile path
  
  -- Count events by reading all pages
  let pageSize = 8192
      numPages = BS.length content `div` pageSize
  
  if numPages == 0
    then return 0
    else do
      -- Count events in all pages
      totalEvents <- countEventsInPages content 0 numPages
      return totalEvents

countEventsInPages :: BS.ByteString -> Int -> Int -> IO Word64
countEventsInPages _ pageIdx numPages | pageIdx >= numPages = return 0
countEventsInPages content pageIdx numPages = do
  let pageOffset = pageIdx * 8192
      pageData = BS.take 8192 $ BS.drop pageOffset content
  
  case EventsPage.fromByteString pageData of
    Left _ -> countEventsInPages content (pageIdx + 1) numPages
    Right page -> do
      let eventsInPage = fromIntegral $ EventsPage.eventCount page
      restEvents <- countEventsInPages content (pageIdx + 1) numPages
      return $ eventsInPage + restEvents

appendEventToFile :: FilePath -> BS.ByteString -> IO ()
appendEventToFile path eventData = do
  -- Read entire file strictly
  content <- BS.readFile path
  
  let pageSize = 8192
      numPages = BS.length content `div` pageSize
  
  if numPages == 0
    then do
      -- Create first page with event
      let page = EventsPage.emptyPage
      case EventsPage.addEvent page eventData of
        Nothing -> return () -- Event too large, ignore
        Just newPage -> BS.writeFile path (EventsPage.toByteString newPage)
    else do
      -- Read last page and try to add event
      let lastPageOffset = (numPages - 1) * pageSize
          lastPageData = BS.drop lastPageOffset content
      case EventsPage.fromByteString (BS.take pageSize lastPageData) of
        Left _ -> return () -- Corrupted page
        Right page -> do
          case EventsPage.addEvent page eventData of
            Just newPage -> do
              -- Update last page
              let beforeLastPage = BS.take lastPageOffset content
                  newContent = beforeLastPage <> EventsPage.toByteString newPage
              BS.writeFile path newContent
            Nothing -> do
              -- Page full, create new page
              let newPage = EventsPage.emptyPage
              case EventsPage.addEvent newPage eventData of
                Just finalPage -> do
                  let newContent = content <> EventsPage.toByteString finalPage
                  BS.writeFile path newContent
                Nothing -> return () -- Event too large

readEventsFromFile :: FilePath -> Word64 -> IO [EventResponse]
readEventsFromFile path startOffset = do
  content <- BS.readFile path
  
  let pageSize = 8192
      numPages = BS.length content `div` pageSize
      
  -- Simple implementation: read all events and filter
  allEvents <- extractAllEvents content 0 numPages 0
  return $ drop (fromIntegral startOffset) allEvents

extractAllEvents :: BS.ByteString -> Int -> Int -> Word64 -> IO [EventResponse]
extractAllEvents _ pageIdx numPages _ | pageIdx >= numPages = return []
extractAllEvents content pageIdx numPages cumulativeOffset = do
  let pageOffset = pageIdx * 8192
      pageData = BS.take 8192 $ BS.drop pageOffset content
  
  case EventsPage.fromByteString pageData of
    Left _ -> extractAllEvents content (pageIdx + 1) numPages cumulativeOffset
    Right page -> do
      let numEvents = EventsPage.eventCount page
          eventsInPage = map (extractEvent page cumulativeOffset) [0 .. numEvents - 1]
          nextOffset = cumulativeOffset + fromIntegral numEvents
      rest <- extractAllEvents content (pageIdx + 1) numPages nextOffset
      return $ eventsInPage ++ rest
  where
    extractEvent page baseOffset idx = 
      let maybeEventBS = EventsPage.event page (fromIntegral idx)
          (eventTypeId, eventDataBS) = case maybeEventBS of
            Nothing -> (0, BS.empty)
            Just eventBS -> 
              if BS.length eventBS > 0 
              then (BS.index eventBS 0, eventBS)
              else (0, BS.empty)
          globalOffset = baseOffset + fromIntegral idx
      in EventResponse
           { responseEventOffset = globalOffset
           , responseEventType = fromIntegral eventTypeId
           , responseEventData = Text.pack $ show eventDataBS
           }
