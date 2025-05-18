module Web.Convent.Storage.EventStoreSpec (spec) where

import Test.Hspec
import Web.Convent.Storage.EventStore
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