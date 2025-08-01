
module Web.Convent.Storage.FilePageSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.FilePage qualified as FilePage
import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempFile)
import System.IO (withFile, IOMode(..))

spec :: Spec
spec = describe "FilePage" $ do
  describe "read/write" $ do
    it "should handle page IO operations" $ do
      withSystemTempFile "test.db" $ \_ handle -> do
        let testData = BS.pack [1,2,3,4,5,6,7,8]
        let pageSize = FilePage.Size 8
        let ix = FilePage.Index 0
        _ <- FilePage.write handle (ix, pageSize) testData
        result <- FilePage.read handle (ix, pageSize)
        result `shouldBe` Right testData

    it "should handle multiple pages" $ do
      withSystemTempFile "test.db" $ \_ handle -> do
        let page0Data = BS.replicate 8 1
        let page1Data = BS.replicate 8 2
        let pageSize = FilePage.Size  8
        _ <- FilePage.write handle (FilePage.Index 0, pageSize) page0Data
        _ <- FilePage.write handle (FilePage.Index 1, pageSize) page1Data
        
        result0 <- FilePage.read handle (FilePage.Index 0, pageSize)
        result1 <- FilePage.read handle (FilePage.Index 1, pageSize)
        
        result0 `shouldBe` Right page0Data
        result1 `shouldBe` Right page1Data

    it "should handle invalid page indices" $ do
      withSystemTempFile "test.db" $ \_ handle -> do
        result <- FilePage.read handle (FilePage.Index (-1), FilePage.Size 8192)
        result `shouldBe` Left (FilePage.ReadInvalidPageIndex (FilePage.Index (-1)))

    it "should handle file too small error" $ do
      withSystemTempFile "test.db" $ \_ handle -> do
        result <- FilePage.read handle (FilePage.Index 1, FilePage.Size 8192)
        result `shouldBe` Left (FilePage.ReadFileTooSmall 16384 0)  -- expecting 16384 (2 pages), got 0 bytes

    it "should handle different page sizes" $ do
      withSystemTempFile "test.db" $ \_ handle -> do
        let testData = BS.replicate 4096 42
        let pageSize = FilePage.Size 4096
        _ <- FilePage.write handle (FilePage.Index 0, pageSize) testData
        result <- FilePage.read handle (FilePage.Index 0, pageSize)
        result `shouldBe` Right testData
