
module Main where

import Test.Hspec
import qualified Web.Convent.Storage.EventStoreSpec as EventStoreSpec
import qualified Web.Convent.Storage.EventsPageSpec as EventsPageSpec
import qualified Web.Convent.Storage.IndexPageSpec as IndexPageSpec
import qualified Web.Convent.Storage.FilePageSpec as FilePageSpec
import qualified Web.Convent.Events.ParticipantLeftEventSpec as ParticipantLeftEventSpec
import qualified Web.Convent.Events.MessageSubmittedEventSpec as MessageSubmittedEventSpec
import qualified Web.Convent.Events.ChatFlowSpec as ChatFlowSpec

main :: IO ()
main = hspec $ do
  describe "EventStore" EventStoreSpec.spec
  describe "EventsPage" EventsPageSpec.spec
  describe "IndexPage" IndexPageSpec.spec
  describe "FilePage" FilePageSpec.spec
  describe "ParticipantLeftEvent" ParticipantLeftEventSpec.spec
  describe "MessageSubmittedEvent" MessageSubmittedEventSpec.spec
  describe "ChatFlow" ChatFlowSpec.spec
