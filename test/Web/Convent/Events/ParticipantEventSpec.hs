
{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.Events.ParticipantJoinedEventSpec (spec) where

import Test.Hspec
import Web.Convent.Events.ParticipantJoinedEvent
import Data.Word (Word64)
import qualified Data.ByteString as BS
import qualified Data.Text as Text

spec :: Spec
spec = describe "ParticipantEvent" $ do
  describe "sanitizeName" $ do
    it "should remove control characters" $ do
      sanitizeName "test\n\r\tname" `shouldBe` "testname"

    it "should truncate names over 30 bytes" $ do
      let longName = Text.replicate 20 "ab"  -- 40 characters
      Text.length (sanitizeName longName) `shouldBe` 30

  describe "encodeParticipantJoined" $ do
    it "should encode event with correct format" $ do
      let event = Event 42 "test"
          encoded = encode event
      BS.index encoded 0 `shouldBe` 0x01  -- event type
      BS.length encoded `shouldBe` 13     -- 1 + 8 + 4 bytes

    it "should handle empty names" $ do
      let event = ParticipantJoinedEvent 42 ""
          encoded = encodeParticipantJoined event
      BS.length encoded `shouldBe` 9      -- 1 + 8 + 0 bytes

  describe "decodeParticipantJoined" $ do
    it "should decode valid event data" $ do
      let event = ParticipantJoinedEvent 42 "test"
          encoded = encodeParticipantJoined event
      decodeParticipantJoined encoded `shouldBe` Just event

    it "should reject invalid event type" $ do
      let invalidData = BS.cons 0x02 $ BS.drop 1 $ encodeParticipantJoined $
            ParticipantJoinedEvent 42 "test"
      decodeParticipantJoined invalidData `shouldBe` Nothing

    it "should reject truncated data" $ do
      let truncated = BS.take 4 $ encodeParticipantJoined $
            ParticipantJoinedEvent 42 "test"
      decodeParticipantJoined truncated `shouldBe` Nothing

    it "should handle maximum length names" $ do
      let name = Text.replicate 30 "a"
          event = ParticipantJoinedEvent 42 name
          encoded = encodeParticipantJoined event
      decodeParticipantJoined encoded `shouldBe` Just event
