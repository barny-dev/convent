{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, withObject, (.:), (.=), object, encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import Text.Read (readMaybe)

data EventItem = EventItem
  { eventOffset :: Integer
  , eventTypeId :: Integer
  , eventData :: Text
  } deriving (Show, Generic)

instance ToJSON EventItem where
  toJSON (EventItem offset typ dat) =
    object ["eventOffset" .= offset, "eventType" .= typ, "eventData" .= dat]

instance FromJSON EventItem where
  parseJSON = withObject "EventItem" $ \v ->
    EventItem <$> v .: "eventOffset"
              <*> v .: "eventType"
              <*> v .: "eventData"

newtype EventsResponse = EventsResponse
  { events :: [EventItem]
  } deriving (Show, Generic)

instance FromJSON EventsResponse where
  parseJSON = withObject "EventsResponse" $ \v -> EventsResponse <$> v .: "events"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [baseUrl, chatIdArg] ->
      runWatch (normalizeBaseUrl baseUrl) chatIdArg 0
    [baseUrl, chatIdArg, startOffsetArg] ->
      case readMaybe startOffsetArg of
        Just startOffset | startOffset >= 0 -> runWatch (normalizeBaseUrl baseUrl) chatIdArg startOffset
        _ -> usage "startOffset must be a non-negative integer."
    _ -> usage "Invalid arguments."

runWatch :: String -> String -> Integer -> IO ()
runWatch baseUrl chatIdArg startOffset =
  case parseChatId chatIdArg of
    Left err -> usage err
    Right chatId -> watchLoop baseUrl chatId startOffset

usage :: String -> IO ()
usage err = do
  hPutStrLn stderr err
  hPutStrLn stderr "Usage: convent-watch <base-url> <chat-id> [start-offset]"
  hPutStrLn stderr "Example: convent-watch http://localhost:8080 550e8400-e29b-41d4-a716-446655440000"

watchLoop :: String -> String -> Integer -> IO ()
watchLoop baseUrl chatId startOffset = loop startOffset
  where
    loop currentOffset = do
      result <- fetchEvents baseUrl chatId currentOffset
      case result of
        Left err -> do
          hPutStrLn stderr err
          threadDelay pollDelayMicros
          loop currentOffset
        Right newEvents -> do
          mapM_ printEvent newEvents
          let nextOffset = case reverse newEvents of
                [] -> currentOffset
                lastEvent : _ -> eventOffset lastEvent + 1
          threadDelay pollDelayMicros
          loop nextOffset

fetchEvents :: String -> String -> Integer -> IO (Either String [EventItem])
fetchEvents baseUrl chatId offset = do
  request <- parseRequest (baseUrl ++ "/chats/" ++ chatId ++ "/events?offset=" ++ show offset)
  response <- httpLBS request
  let code = getResponseStatusCode response
  if code /= 200
    then return $ Left ("HTTP error " ++ show code ++ " while fetching events")
    else case eitherDecode (getResponseBody response) of
      Left parseErr -> return $ Left ("Failed to decode response JSON: " ++ parseErr)
      Right parsed -> return $ Right (events (parsed :: EventsResponse))

printEvent :: EventItem -> IO ()
printEvent event = do
  BL8.putStrLn (encode event)
  hFlush stdout

normalizeBaseUrl :: String -> String
normalizeBaseUrl url =
  let reversed = reverse url
  in case reversed of
       '/':rest -> reverse rest
       _ -> url

parseChatId :: String -> Either String String
parseChatId raw =
  case UUID.fromString raw of
    Nothing -> Left "chatId must be a valid UUID."
    Just uuid -> Right (UUID.toString uuid)

pollDelayMicros :: Int
pollDelayMicros = 1000000
