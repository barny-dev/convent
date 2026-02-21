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
  , calculateNextOffset
  , appendEventToFile
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
joinChat store (ChatId uuid) JoinChatRequest{..} = do
  let chatDir = "chats/" ++ UUID.toString uuid
  
  -- Check if chat exists
  exists <- liftIO $ doesDirectoryExist chatDir
  unless exists $ throwError err404 { errBody = "Chat not found" }
  
  let eventsPath = chatDir ++ "/events.dat"
      indexPath = chatDir ++ "/index.dat"
  
  -- Serialize writes per chat to avoid concurrent offset races
  (pid, eventOffset) <- liftIO $
    ChatStore.withChatLock store uuid $ \_ -> do
      -- Generate participant ID with microsecond precision to avoid collisions
      timestamp <- getCurrentTimestamp
      let pid = timestamp
      
      -- Calculate next event offset using index
      eventOffset <- calculateNextOffset indexPath eventsPath
      
      -- Create and append ParticipantJoinedEvent
      let event = ParticipantJoinedEvent.Event
            { ParticipantJoinedEvent.participantId = pid
            , ParticipantJoinedEvent.timestamp = timestamp
            , ParticipantJoinedEvent.participantName = participantName
            }
      
      result <- appendEventToFile indexPath eventsPath (ParticipantJoinedEvent.encode event)
      case result of
        Left err -> error $ "Failed to append event: " ++ err
        Right () -> return (pid, eventOffset)
  
  return $ JoinChatResponse pid eventOffset

-- | Post a message to a chat
postMessage :: ChatStore -> ChatId -> PostMessageRequest -> Handler PostMessageResponse
postMessage store (ChatId uuid) PostMessageRequest{..} = do
  let chatDir = "chats/" ++ UUID.toString uuid
  
  -- Check if chat exists
  exists <- liftIO $ doesDirectoryExist chatDir
  unless exists $ throwError err404 { errBody = "Chat not found" }
  
  let eventsPath = chatDir ++ "/events.dat"
      indexPath = chatDir ++ "/index.dat"
  
  -- Serialize writes per chat to avoid concurrent offset races
  eventOffsetValue <- liftIO $
    ChatStore.withChatLock store uuid $ \_ -> do
      -- Get current timestamp
      timestamp <- getCurrentTimestamp
      
      -- Calculate next event offset using index
      eventOffset <- calculateNextOffset indexPath eventsPath
      
      -- Create and append MessageSubmittedEvent
      let event = MessageSubmittedEvent.Event
            { MessageSubmittedEvent.participantId = messageParticipantId
            , MessageSubmittedEvent.timestamp = timestamp
            , MessageSubmittedEvent.message = messageText
            }
      
      result <- appendEventToFile indexPath eventsPath (MessageSubmittedEvent.encode event)
      case result of
        Left err -> error $ "Failed to append event: " ++ err
        Right () -> return eventOffset
  
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

-- | Get current timestamp with microsecond precision to avoid collisions
getCurrentTimestamp :: IO Word64
getCurrentTimestamp = do
  posixTime <- getPOSIXTime
  -- Convert to microseconds to avoid collisions within same second
  return $ round (posixTime * 1000000)

-- | Calculate the next event offset by reading the index and last events page
calculateNextOffset :: FilePath -> FilePath -> IO Word64
calculateNextOffset indexPath eventsPath = do
  indexContent <- BS.readFile indexPath
  eventsContent <- BS.readFile eventsPath
  
  let pageSize = 8192
      eventsFileSize = BS.length eventsContent
      
  -- Validate file size is multiple of page size
  if eventsFileSize `mod` pageSize /= 0
    then error $ "Corrupted events file: size " ++ show eventsFileSize ++ " is not a multiple of " ++ show pageSize
    else do
      let numEventsPages = eventsFileSize `div` pageSize
      
      -- Parse the index page
      case IndexPage.fromByteString indexContent of
        Left _ -> return 0 -- Empty or corrupted index
        Right indexPage -> do
          let indexEntries = IndexPage.entries indexPage
          
          if null indexEntries
            then do
              -- No index entries yet, count events in first page (page 0)
              if numEventsPages == 0
                then return 0
                else do
                  let firstPageData = BS.take pageSize eventsContent
                  case EventsPage.fromByteString firstPageData of
                    Left _ -> return 0
                    Right page -> return $ fromIntegral $ EventsPage.eventCount page
            else do
              -- Get the last index entry
              -- Index entries correspond to pages 1, 2, 3, etc. (page 0 has no entry)
              -- If we have N entries, the last tracked page is page N (0-indexed)
              let lastEntry = last indexEntries
                  lastPageIdx = length indexEntries
                  baseOffset = IndexPage.minimumEventOffset lastEntry
              
              -- Read the corresponding events page and count its events
              if lastPageIdx >= numEventsPages
                then return baseOffset -- Page hasn't been created yet
                else do
                  let pageData = BS.take pageSize $ BS.drop (lastPageIdx * pageSize) eventsContent
                  case EventsPage.fromByteString pageData of
                    Left _ -> return baseOffset
                    Right page -> do
                      let eventsInPage = fromIntegral $ EventsPage.eventCount page
                      return $ baseOffset + eventsInPage

appendEventToFile :: FilePath -> FilePath -> BS.ByteString -> IO (Either String ())
appendEventToFile indexPath eventsPath eventData = do
  -- Read entire files strictly
  eventsContent <- BS.readFile eventsPath
  indexContent <- BS.readFile indexPath
  
  let pageSize = 8192
      eventsFileSize = BS.length eventsContent
      
  -- Validate file size is multiple of page size
  if eventsFileSize `mod` pageSize /= 0
    then return $ Left $ "Corrupted events file: size " ++ show eventsFileSize ++ " is not a multiple of " ++ show pageSize
    else do
      let numPages = eventsFileSize `div` pageSize
      
      if numPages == 0
        then do
          -- Create first page with event
          let page = EventsPage.emptyPage
          case EventsPage.addEvent page eventData of
            Nothing -> return $ Left "Event too large for page"
            Just newPage -> do
              BS.writeFile eventsPath (EventsPage.toByteString newPage)
              return $ Right ()
        else do
          -- Read last page and try to add event
          let lastPageOffset = (numPages - 1) * pageSize
              lastPageData = BS.drop lastPageOffset eventsContent
          case EventsPage.fromByteString (BS.take pageSize lastPageData) of
            Left _ -> return $ Left "Corrupted last page"
            Right page -> do
              case EventsPage.addEvent page eventData of
                Just newPage -> do
                  -- Update last page
                  let beforeLastPage = BS.take lastPageOffset eventsContent
                      newContent = beforeLastPage <> EventsPage.toByteString newPage
                  BS.writeFile eventsPath newContent
                  return $ Right ()
                Nothing -> do
                  -- Page full, create new page and update index
                  let newPage = EventsPage.emptyPage
                  case EventsPage.addEvent newPage eventData of
                    Just finalPage -> do
                      -- Calculate the next offset for the new page
                      nextOffset <- calculateNextOffset indexPath eventsPath
                      
                      -- Append new events page
                      let newContent = eventsContent <> EventsPage.toByteString finalPage
                      BS.writeFile eventsPath newContent
                      
                      -- Update index with new entry
                      case IndexPage.fromByteString indexContent of
                        Left _ -> return $ Left "Corrupted index page"
                        Right indexPage -> do
                          case IndexPage.addEntry indexPage nextOffset of
                            Left _ -> return $ Left "Index full or invalid offset"
                            Right newIndexPage -> do
                              BS.writeFile indexPath (IndexPage.toByteString newIndexPage)
                              return $ Right ()
                    Nothing -> return $ Left "Event too large for page"

readEventsFromFile :: FilePath -> Word64 -> IO [EventResponse]
readEventsFromFile path startOffset = do
  content <- BS.readFile path
  
  let pageSize = 8192
      numPages = BS.length content `div` pageSize
      
  -- Simple implementation: read all events and filter
  allEvents <- extractAllEvents content 0 numPages 0
  
  -- Safe conversion of Word64 offset to Int for drop
  let maxDrop :: Int
      maxDrop = maxBound
      safeDrop :: Int
      safeDrop =
        if startOffset > fromIntegral maxDrop
          then maxDrop
          else fromIntegral startOffset
  
  return $ drop safeDrop allEvents

extractAllEvents :: BS.ByteString -> Int -> Int -> Word64 -> IO [EventResponse]
extractAllEvents _ pageIdx numPages _ | pageIdx >= numPages = return []
extractAllEvents content pageIdx numPages cumulativeOffset = do
  let pageOffset = pageIdx * 8192
      pageData = BS.take 8192 $ BS.drop pageOffset content
  
  case EventsPage.fromByteString pageData of
    Left _ -> extractAllEvents content (pageIdx + 1) numPages cumulativeOffset
    Right page -> do
      let numEventsWord = EventsPage.eventCount page
          numEvents :: Int
          numEvents = fromIntegral numEventsWord
          eventsInPage =
            if numEventsWord == 0
              then []
              else map (extractEvent page cumulativeOffset) [0 .. numEvents - 1]
          nextOffset = cumulativeOffset + fromIntegral numEventsWord
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
