
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

    it "should reject non-ascending event offsets" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,10],  -- second entry
            BS.replicate (8192 - 16) 0]
      fromByteString page `shouldBe` Left (NonAscendingEventOffsetError 1)

    it "should reject non-zero trailing entries" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.replicate 8 0,  -- zero entry
            BS.pack [0,0,0,0,0,0,0,40],  -- invalid trailing entry
            BS.replicate (8192 - 24) 0]
      fromByteString page `shouldBe` Left (NonZeroTrailingEntryError 2)

  describe "entryCount" $ do
    it "should count entries correctly" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 16) 0]
      case fromByteString page of
        Right indexPage -> entryCount indexPage `shouldBe` 2
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "entries" $ do
    it "should return all non-zero entries" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,30],  -- second entry
            BS.replicate (8192 - 16) 0]
      case fromByteString page of
        Right indexPage -> entries indexPage `shouldBe` [
          IndexEntry 20,
          IndexEntry 30
          ]
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "entry" $ do
    it "should return specific entry" $ do
      let page = BS.concat [
            BS.pack [0,0,0,0,0,0,0,20],  -- first entry
            BS.pack [0,0,0,0,0,0,0,30],  -- second entry 
            BS.replicate (8192 - 16) 0]
      case fromByteString page of
        Right indexPage -> do
          entry indexPage 0 `shouldBe` IndexEntry 20
          entry indexPage 1 `shouldBe` IndexEntry 30
        Left err -> expectationFailure $ "Failed to create test page: " ++ show err

  describe "addEntry" $ do
    it "should add entry to empty page" $ do
      let result = addEntry emptyPage 20
      case result of
        Nothing -> expectationFailure "Failed to add entry to empty page"
        Just page -> do
          entryCount page `shouldBe` 1
          entry page 0 `shouldBe` IndexEntry 20

    it "should maintain ascending order" $ do
      let page1 = case addEntry emptyPage 20 of
            Nothing -> error "Failed to add first entry"
            Just p -> p
      let page2 = case addEntry page1 30 of
            Nothing -> error "Failed to add second entry"
            Just p -> p
      entryCount page2 `shouldBe` 2
      entries page2 `shouldBe` [IndexEntry 20, IndexEntry 30]

    it "should reject non-ascending entries" $ do
      let page = case addEntry emptyPage 20 of
            Nothing -> error "Failed to add first entry"
            Just p -> p
      addEntry page 10 `shouldBe` Nothing

    it "should reject entries when page is full" $ do
      let addEntries n p = if n <= 0 then Just p else do
            p' <- addEntry p (fromIntegral $ 10241 - n * 10)
            addEntries (n - 1) p'
      let fullPage = addEntries 1024 emptyPage
      case fullPage of
        Nothing -> expectationFailure "Failed to create full page"
        Just page -> do
          entryCount page `shouldBe` 1024
          addEntry page 9999 `shouldBe` Nothing
