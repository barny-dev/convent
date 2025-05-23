
module Main where

import Test.Hspec
import qualified Web.Convent.Storage.EventStoreSpec as EventStoreSpec
import qualified Web.Convent.Storage.EventsPageSpec as EventsPageSpec
import qualified Web.Convent.Storage.IndexPageSpec as IndexPageSpec
import qualified Web.Convent.Events.ParticipantLeftEventSpec as ParticipantLeftEventSpec

main :: IO ()
main = hspec $ do
  describe "EventStore" EventStoreSpec.spec
  describe "EventsPage" EventsPageSpec.spec
  describe "IndexPage" IndexPageSpec.spec
  describe "ParticipantLeftEvent" ParticipantLeftEventSpec.spec
