
module Web.Convent.Util.ByteString
  ( readW16BE
  , readW64BE
  , writeW64BE
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word16, Word64)
import Data.Bits (shiftL, shiftR)

readW16BE :: ByteString -> Int -> Word16
readW16BE bs offset =
  let hi = (fromIntegral $ ByteString.index bs offset) * 256
      lo = fromIntegral $ ByteString.index bs (offset + 1)
   in hi + lo

readW64BE :: ByteString -> Int -> Word64
readW64BE bs offset =
  let b0 = fromIntegral (ByteString.index bs offset) `shiftL` 56
      b1 = fromIntegral (ByteString.index bs (offset + 1)) `shiftL` 48
      b2 = fromIntegral (ByteString.index bs (offset + 2)) `shiftL` 40
      b3 = fromIntegral (ByteString.index bs (offset + 3)) `shiftL` 32
      b4 = fromIntegral (ByteString.index bs (offset + 4)) `shiftL` 24
      b5 = fromIntegral (ByteString.index bs (offset + 5)) `shiftL` 16
      b6 = fromIntegral (ByteString.index bs (offset + 6)) `shiftL` 8
      b7 = fromIntegral (ByteString.index bs (offset + 7))
   in b0 + b1 + b2 + b3 + b4 + b5 + b6 + b7


writeW64BE :: Word64 -> ByteString
writeW64BE value = ByteString.pack
  [ fromIntegral (value `shiftR` 56)
  , fromIntegral (value `shiftR` 48)
  , fromIntegral (value `shiftR` 40)
  , fromIntegral (value `shiftR` 32)
  , fromIntegral (value `shiftR` 24)
  , fromIntegral (value `shiftR` 16)
  , fromIntegral (value `shiftR` 8)
  , fromIntegral value
  ]

