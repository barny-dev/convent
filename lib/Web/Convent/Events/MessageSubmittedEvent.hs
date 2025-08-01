
{-# LANGUAGE RecordWildCards #-}
module Web.Convent.Events.MessageSubmittedEvent
  ( Event(..)
  , eventType
  , encode
  , decode
  , sanitizeMessage
  ) where

import Data.Word (Word8, Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Char (isControl)
import Web.Convent.Util.ByteString (readW64BE, writeW64BE)

data Event = Event
  { participantId :: Word64
  , timestamp :: Word64
  , message :: Text
  } deriving (Show, Eq)

eventType :: Word8
eventType = 0x03

sanitizeMessage :: Text -> Text
sanitizeMessage = Text.take 1000 . Text.filter (not . isControl)

encode :: Event -> ByteString
encode Event{..} =
  let messageBytes = Text.encodeUtf8 $ sanitizeMessage message
   in BS.concat [BS.singleton eventType, writeW64BE timestamp, writeW64BE participantId, messageBytes]

decode :: ByteString -> Maybe Event
decode bytes =
  if BS.length bytes < 17 then Nothing
  else
    let eventTypeByte = BS.index bytes 0
        timestampBytes = BS.take 8 $ BS.drop 1 bytes
        idBytes = BS.take 8 $ BS.drop 9 bytes
        messageBytes = BS.drop 17 bytes
    in if eventTypeByte /= eventType then Nothing
       else Just Event
         { participantId = readW64BE idBytes 0
         , timestamp = readW64BE timestampBytes 0
         , message = Text.decodeUtf8 messageBytes
         }
