
module Web.Convent.Storage.EventsPageSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.EventsPage
import qualified Data.ByteString as BS
import Data.Word (Word16)

spec :: Spec
spec = describe "EventsPage" $ do
  describe "emptyPage" $ do
    it "should create a valid empty page" $ do
      let page = emptyPage
      reservePtr page `shouldBe` 2
      toByteString page `shouldSatisfy` \bs -> BS.length bs == 8192

  describe "fromByteString/toByteString" $ do
    it "should handle valid page data" $ do
      let page = emptyPage
      let bytes = toByteString page
      fromByteString bytes `shouldBe` Right page

    it "should reject invalid page size" $ do
      fromByteString (BS.replicate 100 0) `shouldBe` Left (InvalidPageSizeError 100)

    it "should reject invalid reserve pointer" $ do
      let invalidPage = BS.replicate 8192 0
      fromByteString invalidPage `shouldBe` Left (InvalidReservePtrError 0)

  describe "event operations" $ do
    let testEvent = BS.pack [1,2,3,4]
    let pageWithEvent = do
          page <- addEvent emptyPage testEvent
          return page
    
    it "should correctly calculate event count" $ do
      case pageWithEvent of
        Nothing -> expectationFailure "Failed to add event"
        Just page -> eventCount page `shouldBe` 1

    it "should add and retrieve events" $ do
      case pageWithEvent of
        Nothing -> expectationFailure "Failed to add event"
        Just page -> do
          event page 0 `shouldBe` Just testEvent
          eventCopy page 0 `shouldBe` Just testEvent
          eventPtr page 0 `shouldBe` Just (8192 - fromIntegral (BS.length testEvent))

    it "should handle invalid event index" $ do
      case pageWithEvent of
        Nothing -> expectationFailure "Failed to add event"
        Just page -> do
          event page 1 `shouldBe` Nothing
          eventCopy page 1 `shouldBe` Nothing
          eventPtr page 1 `shouldBe` Nothing

  describe "reserve space" $ do
    it "should calculate correct reserve space" $ do
      let page = emptyPage
      reserve page `shouldBe` 8190

    it "should update reserve after adding event" $ do
      let testEvent = BS.pack [1,2,3,4]
      case addEvent emptyPage testEvent of
        Nothing -> expectationFailure "Failed to add event"
        Just page -> reserve page `shouldBe` (8190 - fromIntegral (BS.length testEvent) - 2)
