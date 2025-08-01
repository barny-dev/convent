
{-# LANGUAGE RecordWildCards #-}
module Web.Convent.Events.ParticipantLeftEvent
  ( Event(..)
  , eventType
  , encode
  , decode
  ) where

import Data.Word (Word8, Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Web.Convent.Util.ByteString (readW64BE, writeW64BE)

data Event = Event
  { participantId :: Word64
  , timestamp :: Word64
  } deriving (Show, Eq)

eventType :: Word8
eventType = 0x02

encode :: Event -> ByteString
encode Event{..} = BS.concat [BS.singleton eventType, writeW64BE timestamp, writeW64BE participantId]

decode :: ByteString -> Maybe Event
decode bytes =
  if BS.length bytes < 17 then Nothing
  else
    let eventTypeByte = BS.index bytes 0
        timestampBytes = BS.take 8 $ BS.drop 1 bytes
        idBytes = BS.take 8 $ BS.drop 9 bytes
    in if eventTypeByte /= eventType then Nothing
       else Just Event
         { participantId = readW64BE idBytes 0
         , timestamp = readW64BE timestampBytes 0
         }
