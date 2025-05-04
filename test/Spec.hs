
module Main where

import Test.Hspec
import qualified Web.Convent.Storage.EventStoreSpec as EventStoreSpec

main :: IO ()
main = hspec $ do
  describe "EventStore" EventStoreSpec.spec
