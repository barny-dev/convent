module Web.Convent.Storage.EventStoreSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.EventStore
import qualified Data.ByteString as BS
import Data.Word (Word16)
import System.IO.Temp (withSystemTempFile)
import System.IO (withFile, IOMode(..))

spec :: Spec
spec = describe "EventStore" $ do
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

  describe "segment operations" $ do
    let testSegment = BS.pack [1,2,3,4]
    let pageWithSegment = do
          page <- addSegment emptyPage testSegment
          return page
    it "should correctly calculate segment count" $ do
      case pageWithSegment of
        Nothing -> expectationFailure "Failed to add segment"
        Just page -> do
          segmentCount page `shouldBe` 1
    it "should add and retrieve segments" $ do
      case pageWithSegment of
        Nothing -> expectationFailure "Failed to add segment"
        Just page -> do
          segment page 0 `shouldBe` Just testSegment
          segmentCopy page 0 `shouldBe` Just testSegment
          segmentPtr page 0 `shouldBe` Just (8192 - fromIntegral (BS.length testSegment))

    it "should handle invalid segment index" $ do
      case pageWithSegment of
        Nothing -> expectationFailure "Failed to add segment"
        Just page -> do
          segment page 1 `shouldBe` Nothing
          segmentCopy page 1 `shouldBe` Nothing
          segmentPtr page 1 `shouldBe` Nothing

  describe "reserve space" $ do
    it "should calculate correct reserve space" $ do
      let page = emptyPage
      reserve page `shouldBe` 8190

    it "should update reserve after adding segment" $ do
      let testSegment = BS.pack [1,2,3,4]
      case addSegment emptyPage testSegment of
        Nothing -> expectationFailure "Failed to add segment"
        Just page -> do
          reserve page `shouldBe` (8190 - fromIntegral (BS.length testSegment) - 2)

  describe "file operations" $ do
    it "should write and read page" $ do
      withSystemTempFile "test.dat" $ \path handle -> do
        let page = emptyPage
        writePage handle 0 page
        result <- readPage handle 0
        result `shouldBe` Right page

    it "should handle reading beyond file size" $ do
      withSystemTempFile "test.dat" $ \path handle -> do
        result <- readPage handle 1
        result `shouldBe` Left ReadFileTooSmallError

    it "should write and read multiple pages" $ do
      withSystemTempFile "test.dat" $ \path handle -> do
        let page1 = emptyPage
        let page2 = case addSegment emptyPage (BS.pack [1,2,3,4]) of
                     Just p -> p
                     Nothing -> error "Failed to create test page"
        
        writePage handle 0 page1
        writePage handle 1 page2
        
        result1 <- readPage handle 0
        result2 <- readPage handle 1
        
        result1 `shouldBe` Right page1
        result2 `shouldBe` Right page2
