
{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.Events.MessageSubmittedEventSpec (spec) where

import Test.Hspec
import Web.Convent.Events.MessageSubmittedEvent
import Data.Word (Word64)
import qualified Data.ByteString as BS
import qualified Data.Text as Text

spec :: Spec
spec = describe "MessageSubmittedEvent" $ do
  describe "sanitizeMessage" $ do
    it "should remove control characters" $ do
      sanitizeMessage "hello\n\r\tworld" `shouldBe` "helloworld"

    it "should truncate messages over 1000 bytes" $ do
      let longMessage = Text.replicate 600 "ab"  -- 1200 characters
      Text.length (sanitizeMessage longMessage) `shouldBe` 1000

  describe "encode/decode" $ do
    it "should encode and decode event correctly" $ do
      let event = Event 42 1234567890 "Hello, World!"
          encoded = encode event
      BS.index encoded 0 `shouldBe` 0x03  -- event type
      BS.length encoded `shouldBe` 30     -- 1 + 8 + 8 + 13 bytes
      decode encoded `shouldBe` Just event

    it "should reject invalid event type" $ do
      let invalidData = BS.cons 0x01 $ BS.drop 1 $ encode $ Event 42 1234567890 "test"
      decode invalidData `shouldBe` Nothing

    it "should reject truncated data" $ do
      let truncated = BS.take 4 $ encode $ Event 42 1234567890 "test"
      decode truncated `shouldBe` Nothing

    it "should handle maximum length messages" $ do
      let msg = Text.replicate 1000 "a"
          event = Event 42 1234567890 msg
          encoded = encode event
      decode encoded `shouldBe` Just event
