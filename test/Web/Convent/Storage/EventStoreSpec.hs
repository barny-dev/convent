
module Web.Convent.Storage.EventStoreSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.EventStore
import qualified Data.ByteString as BS
import Data.Word (Word16)
import System.IO.Temp (withSystemTempFile)
import System.IO (withFile, IOMode(..))

spec :: Spec
spec = describe "EventStore" $ do
  describe "readPage" $ do
    it "should load the second page filled with ones" $ do
      withSystemTempFile "test.dat" $ \path handle -> do
        -- Create first page of zeros
        BS.hPut handle (BS.replicate 8192 0)
        -- Create second page of ones
        BS.hPut handle (BS.replicate 8192 1)
        
        result <- readPage handle 1
        case result of
          Right page -> do
            BS.length page `shouldBe` 8192
            all (== 1) (BS.unpack page) `shouldBe` True
          Left err -> fail $ "Expected Right but got Left: " ++ show err

  describe "parsePage" $ do
    let makePage reserve ptrs = 
          let headerBS = BS.pack $ concatMap word16ToBytes (reserve:ptrs)
              padding = BS.replicate (8192 - BS.length headerBS) 0
          in headerBS <> padding
        word16ToBytes w = [fromIntegral (w `div` 256), fromIntegral (w `mod` 256)]

    it "should parse valid page with multiple pointers" $ do
      let page = makePage 100 [80, 60, 40, 20]
      case parsePage page of
        Right p -> do
          reservePtr p `shouldBe` 100
          pointers p `shouldBe` [80, 60, 40, 20]
          length (segments p) `shouldBe` 4
        Left err -> fail $ "Expected Right but got Left: " ++ show err

    it "should parse valid page with one pointer" $ do
      let page = makePage 50 [30]
      case parsePage page of
        Right p -> do
          reservePtr p `shouldBe` 50
          pointers p `shouldBe` [30]
          length (segments p) `shouldBe` 1
        Left err -> fail $ "Expected Right but got Left: " ++ show err

    it "should reject page with reserve pointer >= 8192" $ do
      let page = makePage 8192 [80, 60]
      parsePage page `shouldBe` Left InvalidReservePointer

    it "should reject page with non-descending pointers" $ do
      let page = makePage 100 [80, 90, 40]
      case parsePage page of
        Right p -> do
          pointers p `shouldBe` [80]
        Left err -> fail $ "Expected Right but got Left: " ++ show err

    it "should allow last pointer equal to reserve pointer" $ do
      let page = makePage 100 [80, 100]
      case parsePage page of
        Right p -> do
          reservePtr p `shouldBe` 100
          pointers p `shouldBe` [80, 100]
          length (segments p) `shouldBe` 2
        Left err -> fail $ "Expected Right but got Left: " ++ show err
