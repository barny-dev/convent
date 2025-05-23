
{-# LANGUAGE RecordWildCards #-}
module Web.Convent.Events.ParticipantJoinedEvent
  ( Event(..)
  , eventType
  , encode
  , decode
  , sanitizeName
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
  , participantName :: Text
  } deriving (Show, Eq)

eventType :: Word8
eventType = 0x01

sanitizeName :: Text -> Text
sanitizeName = Text.take 30 . Text.filter (not . isControl)

encode :: Event -> ByteString
encode Event{..} =
  let nameBytes = Text.encodeUtf8 $ sanitizeName participantName
   in BS.concat [BS.singleton eventType, writeW64BE timestamp, writeW64BE participantId, nameBytes]

decode :: ByteString -> Maybe Event
decode bytes =
  if BS.length bytes < 17 then Nothing
  else
    let eventTypeByte = BS.index bytes 0
        timestampBytes = BS.take 8 $ BS.drop 1 bytes
        idBytes = BS.take 8 $ BS.drop 9 bytes
        nameBytes = BS.drop 17 bytes
    in if eventTypeByte /= eventType then Nothing
       else Just Event
         { participantId = readW64BE idBytes 0
         , timestamp = readW64BE timestampBytes 0
         , participantName = Text.decodeUtf8 nameBytes
         }
