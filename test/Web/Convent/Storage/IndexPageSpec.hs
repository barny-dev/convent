
module Web.Convent.Storage.IndexPageSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.IndexPage qualified as IndexPage
import qualified Data.ByteString as BS
import Data.Word (Word64)

spec :: Spec
spec = describe "IndexPage" $ do
  describe "fromByteString" $ do
    it "should reject pages of invalid size" $ do
      IndexPage.fromByteString (BS.replicate 100 0) `shouldBe` Left (IndexPage.InvalidPageSizeError 100)

    it "should accept empty index page" $ do
      let page = BS.replicate 8192 0
      IndexPage.fromByteString page `shouldBe` Right IndexPage.emptyPage

    it "should reject non-ascending event offsets" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,10],  -- second entry
            BS.replicate (8192 - 16) 0]
      IndexPage.fromByteString page `shouldBe` Left (IndexPage.NonAscendingEventOffsetError 1)

    it "should reject non-zero trailing entries" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.replicate 8 0,  -- zero entry
            BS.pack [0,0,0,0,0,0,0,40],  -- invalid trailing entry
            BS.replicate (8192 - 24) 0]
      IndexPage.fromByteString page `shouldBe` Left (IndexPage.NonZeroTrailingEntryError 2)

  describe "entryCount" $ do
    it "should count entries correctly" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 16) 0]
      case IndexPage.fromByteString page of
        Right indexPage -> IndexPage.entryCount indexPage `shouldBe` 2
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "entries" $ do
    it "should return all non-zero entries" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 16) 0]
      case IndexPage.fromByteString page of
        Right indexPage -> IndexPage.entries indexPage `shouldBe` [
          IndexPage.IndexEntry 20,
          IndexPage.IndexEntry 30
          ]
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "entry" $ do
    it "should return specific entry" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,30],  -- second entry 
            BS.replicate (8192 - 16) 0]
      case IndexPage.fromByteString page of
        Right indexPage -> do
          IndexPage.entry indexPage 0 `shouldBe` IndexPage.IndexEntry 20
          IndexPage.entry indexPage 1 `shouldBe` IndexPage.IndexEntry 30
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "addEntry" $ do
    it "should add entry to empty page" $ do
      let result = IndexPage.addEntry IndexPage.emptyPage 20
      case result of
        Left err -> expectationFailure $ "Failed to add entry to empty page: " ++ show err
        Right page -> do
          IndexPage.entryCount page `shouldBe` 1
          IndexPage.entry page 0 `shouldBe` IndexPage.IndexEntry 20

    it "should maintain ascending order" $ do
      let page1 = case IndexPage.addEntry IndexPage.emptyPage 20 of
            Left err -> error $ "Failed to add first entry: " ++ show err
            Right p -> p
      let page2 = case IndexPage.addEntry page1 30 of
            Left err -> error $ "Failed to add second entry: " ++ show err
            Right p -> p
      IndexPage.entryCount page2 `shouldBe` 2
      IndexPage.entries page2 `shouldBe` [IndexPage.IndexEntry 20, IndexPage.IndexEntry 30]

    it "should reject non-ascending entries" $ do
      let page = case IndexPage.addEntry IndexPage.emptyPage 20 of
            Left err -> error $ "Failed to add first entry: " ++ show err
            Right p -> p
      IndexPage.addEntry page 10 `shouldBe` Left IndexPage.NonAscendingOffset

    it "should reject entries when page is full" $ do
      let addEntries n p = if n <= 0 then Right p else do
            p' <- IndexPage.addEntry p (fromIntegral $ 10241 - n * 10)
            addEntries (n - 1) p'
      let fullPage = addEntries 1024 IndexPage.emptyPage
      case fullPage of
        Left err -> expectationFailure $ "Failed to create full page: " ++ show err
        Right page -> do
          IndexPage.entryCount page `shouldBe` 1024
          IndexPage.addEntry page 9999 `shouldBe` Left IndexPage.PageFull
