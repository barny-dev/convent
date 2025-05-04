
module Web.Convent.Storage.Utils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word16)
import Data.Bits (shift, (.|.))

readWord16be :: ByteString -> Int -> Word16
readWord16be bs idx =
  let high = fromIntegral $ ByteString.index bs idx
      low = fromIntegral $ ByteString.index bs (idx + 1)
  in shift high 8 .|. low
