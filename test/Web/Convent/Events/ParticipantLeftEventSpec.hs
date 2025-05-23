
{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.Events.ParticipantLeftEventSpec (spec) where

import Test.Hspec
import Web.Convent.Events.ParticipantLeftEvent
import qualified Data.ByteString as BS

spec :: Spec
spec = describe "ParticipantLeftEvent" $ do
  describe "encode/decode" $ do
    it "should encode and decode event correctly" $ do
      let event = Event 42 1234567890
          encoded = encode event
      BS.index encoded 0 `shouldBe` 0x02  -- event type
      BS.length encoded `shouldBe` 17     -- 1 + 8 + 8 bytes
      decode encoded `shouldBe` Just event

    it "should reject invalid event type" $ do
      let invalidData = BS.cons 0x01 $ BS.drop 1 $ encode $ Event 42 1234567890
      decode invalidData `shouldBe` Nothing

    it "should reject truncated data" $ do
      let truncated = BS.take 4 $ encode $ Event 42 1234567890
      decode truncated `shouldBe` Nothing
