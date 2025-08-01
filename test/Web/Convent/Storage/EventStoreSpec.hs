
module Web.Convent.Storage.EventStoreSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.EventStore
import Web.Convent.Storage.EventsPage
import qualified Data.ByteString as BS
import Data.Word (Word16)
import System.IO.Temp (withSystemTempFile)
import System.IO (withFile, IOMode(..))

spec :: Spec
spec = describe "EventStore" $ do
  describe "readPage/writePage" $ do
    it "should handle page IO operations" $ do
      withSystemTempFile "test.db" $ \path handle -> do
        let testEvent = BS.pack [1,2,3,4]
        let page = case addEvent emptyPage testEvent of
              Nothing -> error "Failed to create test page"
              Just p -> p

        writePage handle 0 page
        result <- readPage handle 0
        result `shouldBe` Right page

    it "should handle invalid page indices" $ do
      withSystemTempFile "test.db" $ \path handle -> do
        result <- readPage handle (-1)
        result `shouldBe` Left (InvalidPageOffset (-1))

    it "should handle file too small error" $ do
      withSystemTempFile "test.db" $ \path handle -> do
        result <- readPage handle 1
        result `shouldBe` Left (FileTooSmall 16384 0)  -- expecting 16384 (2 pages), got 0 bytes

    it "should wrap EventsPage errors" $ do
      withSystemTempFile "test.db" $ \path handle -> do
        BS.hPut handle (BS.replicate 8192 0)  -- Write invalid page data
        result <- readPage handle 0
        case result of
          Left (PageError _) -> return ()  -- Success - we got a wrapped PageReadError
          other -> expectationFailure $ "Expected PageError, got: " ++ show other
