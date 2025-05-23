
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
  , participantName :: Text
  } deriving (Show, Eq)

eventType :: Word8
eventType = 0x01

-- | Sanitize name by removing control characters and truncating to 30 bytes
sanitizeName :: Text -> Text
sanitizeName = Text.take 30 . Text.filter (not . isControl)

-- | Encode a participant joined event into bytes
encode :: Event -> ByteString
encode Event{..} =
  let nameBytes = Text.encodeUtf8 $ sanitizeName participantName
   in BS.concat [BS.singleton eventType, writeW64BE participantId, nameBytes]

-- | Decode a participant joined event from bytes
decode :: ByteString -> Maybe Event
decode bytes =
  if BS.length bytes < 9 then Nothing
  else
    let eventTypeByte = BS.index bytes 0
        idBytes = BS.take 8 $ BS.drop 1 bytes
        nameBytes = BS.drop 9 bytes
    in if eventTypeByte /= eventType then Nothing
       else Just Event
         { participantId = readW64BE idBytes 0
         , participantName = Text.decodeUtf8 nameBytes
         }
