
{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.Events.ParticipantJoinedEventSpec (spec) where

import Test.Hspec
import Web.Convent.Events.ParticipantJoinedEvent
import Data.Word (Word64)
import qualified Data.ByteString as BS
import qualified Data.Text as Text

spec :: Spec
spec = describe "ParticipantJoinedEvent" $ do
  describe "sanitizeName" $ do
    it "should remove control characters" $ do
      sanitizeName "test\n\r\tname" `shouldBe` "testname"

    it "should truncate names over 30 bytes" $ do
      let longName = Text.replicate 20 "ab"  -- 40 characters
      Text.length (sanitizeName longName) `shouldBe` 30

  describe "encode/decode" $ do
    it "should encode and decode event correctly" $ do
      let event = Event 42 1234567890 "test"
          encoded = encode event
      BS.index encoded 0 `shouldBe` 0x01  -- event type
      BS.length encoded `shouldBe` 21     -- 1 + 8 + 8 + 4 bytes
      decode encoded `shouldBe` Just event

    it "should reject invalid event type" $ do
      let invalidData = BS.cons 0x02 $ BS.drop 1 $ encode $ Event 42 1234567890 "test"
      decode invalidData `shouldBe` Nothing

    it "should reject truncated data" $ do
      let truncated = BS.take 4 $ encode $ Event 42 1234567890 "test"
      decode truncated `shouldBe` Nothing

    it "should handle maximum length names" $ do
      let name = Text.replicate 30 "a"
          event = Event 42 1234567890 name
          encoded = encode event
      decode encoded `shouldBe` Just event
