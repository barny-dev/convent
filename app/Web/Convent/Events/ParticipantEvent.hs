
{-# LANGUAGE RecordWildCards #-}
module Web.Convent.Events.ParticipantEvent
  ( ParticipantJoinedEvent(..)
  , encodeParticipantJoined
  , decodeParticipantJoined
  , sanitizeName
  ) where

import Data.Word (Word8, Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Char (isControl)

data ParticipantJoinedEvent = ParticipantJoinedEvent
  { participantId :: Word64
  , participantName :: Text
  } deriving (Show, Eq)

eventTypeParticipantJoined :: Word8
eventTypeParticipantJoined = 0x01

-- | Sanitize name by removing control characters and truncating to 30 bytes
sanitizeName :: Text -> Text
sanitizeName = Text.take 30 . Text.filter (not . isControl)

-- | Encode a participant joined event into bytes
encodeParticipantJoined :: ParticipantJoinedEvent -> ByteString
encodeParticipantJoined ParticipantJoinedEvent{..} =
  let nameBytes = Text.encodeUtf8 $ sanitizeName participantName
      idBytes = BS.pack $ word64ToBytes participantId
   in BS.concat [BS.singleton eventTypeParticipantJoined, idBytes, nameBytes]
  where
    word64ToBytes :: Word64 -> [Word8]
    word64ToBytes n = map fromIntegral
      [ n `shiftR` 56
      , n `shiftR` 48
      , n `shiftR` 40
      , n `shiftR` 32
      , n `shiftR` 24
      , n `shiftR` 16
      , n `shiftR` 8
      , n
      ]

-- | Decode a participant joined event from bytes
decodeParticipantJoined :: ByteString -> Maybe ParticipantJoinedEvent
decodeParticipantJoined bytes =
  if BS.length bytes < 9 then Nothing
  else
    let eventType = BS.index bytes 0
        idBytes = BS.take 8 $ BS.drop 1 bytes
        nameBytes = BS.drop 9 bytes
    in if eventType /= eventTypeParticipantJoined then Nothing
       else Just ParticipantJoinedEvent
         { participantId = bytesToWord64 $ BS.unpack idBytes
         , participantName = Text.decodeUtf8 nameBytes
         }
  where
    bytesToWord64 :: [Word8] -> Word64
    bytesToWord64 = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0
