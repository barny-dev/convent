
module Web.Convent.Storage.IndexPageSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.IndexPage
import qualified Data.ByteString as BS
import Data.Word (Word64)

spec :: Spec
spec = describe "IndexPage" $ do
  describe "fromByteString" $ do
    it "should reject pages of invalid size" $ do
      fromByteString (BS.replicate 100 0) `shouldBe` Left (InvalidPageSizeError 100)

    it "should accept empty index page" $ do
      let page = BS.replicate 8192 0
      fromByteString page `shouldBe` Right emptyPage

    it "should reject non-ascending page offsets" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20], BS.pack [0,0,0,0,0,0,0,1],  -- first segment
            BS.pack [0,0,0,0,0,0,0,10], BS.pack [0,0,0,0,0,0,0,2],  -- second segment
            BS.replicate (8192 - 32) 0]
      fromByteString page `shouldBe` Left (NonAscendingOffsetError 1)

    it "should reject non-ascending event offsets" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,10], BS.pack [0,0,0,0,0,0,0,20],  -- first segment
            BS.pack [0,0,0,0,0,0,0,20], BS.pack [0,0,0,0,0,0,0,10],  -- second segment
            BS.replicate (8192 - 32) 0]
      fromByteString page `shouldBe` Left (NonAscendingEventOffsetError 1)

    it "should reject non-zero trailing segments" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,10], BS.pack [0,0,0,0,0,0,0,20],  -- first segment
            BS.replicate 16 0,  -- zero segment
            BS.pack [0,0,0,0,0,0,0,30], BS.pack [0,0,0,0,0,0,0,40],  -- invalid trailing segment
            BS.replicate (8192 - 48) 0]
      fromByteString page `shouldBe` Left (NonZeroTrailingSegmentError 2)

  describe "entryCount" $ do
    it "should count entries correctly" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,10], BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,20], BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 32) 0]
      case fromByteString page of
        Right indexPage -> entryCount indexPage `shouldBe` 2
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "entries" $ do
    it "should return all non-zero entries" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,10], BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,20], BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 32) 0]
      case fromByteString page of
        Right indexPage -> entries indexPage `shouldBe` [
          IndexEntry 10 20,
          IndexEntry 20 30
          ]
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "entry" $ do
    it "should return specific entry" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,10], BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,20], BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 32) 0]
      case fromByteString page of
        Right indexPage -> do
          entry indexPage 0 `shouldBe` IndexEntry 10 20
          entry indexPage 1 `shouldBe` IndexEntry 20 30
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err
