
module Main where

import Test.Hspec
import qualified Web.Convent.Storage.EventStoreSpec as EventStoreSpec
import qualified Web.Convent.Storage.IndexPageSpec as IndexPageSpec

main :: IO ()
main = hspec $ do
  describe "EventStore" EventStoreSpec.spec
  describe "IndexPage" IndexPageSpec.spec
