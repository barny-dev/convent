
module Web.Convent.Storage.EventStoreSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.EventStore
import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempFile)
import System.IO (withFile, IOMode(..))

spec :: Spec
spec = describe "EventStore" $ do
  describe "loadPage" $ do
    it "should load the second page filled with ones" $ do
      withSystemTempFile "test.dat" $ \path handle -> do
        -- Create first page of zeros
        BS.hPut handle (BS.replicate 8192 0)
        -- Create second page of ones
        BS.hPut handle (BS.replicate 8192 1)
        
        -- Close the file and reopen it for reading
        withFile path ReadMode $ \h -> do
          result <- loadPage 1 h
          case result of
            Right page -> do
              BS.length page `shouldBe` 8192
              all (== 1) (BS.unpack page) `shouldBe` True
            Left err -> fail $ "Expected Right but got Left: " ++ show err
