
{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.Events.ChatFlowSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Web.Convent.Events.ParticipantJoinedEvent as Join
import qualified Web.Convent.Events.MessageSubmittedEvent as Message
import qualified Web.Convent.Events.ParticipantLeftEvent as Leave
import qualified Web.Convent.Storage.EventsPage as EventsPage

-- | Tests a complete chat flow: join, message, leave
spec :: Spec
spec = describe "ChatFlow" $ do
  describe "participant join, message, leave sequence" $ do
    it "should store and retrieve all events in sequence" $ do
      -- Create events for a chat session
      let participantId = 12345
          timestamp1 = 1700000000
          timestamp2 = 1700000001
          timestamp3 = 1700000002

          -- Participant joins the chat
          joinEvent = Join.Event 
            { Join.participantId = participantId
            , Join.timestamp = timestamp1
            , Join.participantName = "Alice"
            }

          -- Participant sends a message
          messageEvent = Message.Event 
            { Message.participantId = participantId
            , Message.timestamp = timestamp2
            , Message.message = "Hello, World!"
            }

          -- Participant leaves the chat
          leaveEvent = Leave.Event 
            { Leave.participantId = participantId
            , Leave.timestamp = timestamp3
            }

      -- Encode all events
      let joinEncoded = Join.encode joinEvent
          messageEncoded = Message.encode messageEvent
          leaveEncoded = Leave.encode leaveEvent

      -- Create page and add events
      let page0 = EventsPage.emptyPage
          page1 = fromJust $ EventsPage.addEvent page0 joinEncoded
          page2 = fromJust $ EventsPage.addEvent page1 messageEncoded
          page3 = fromJust $ EventsPage.addEvent page2 leaveEncoded

      -- Verify event count
      EventsPage.eventCount page3 `shouldBe` 3

      -- Retrieve and decode events
      let retrievedJoin = fromJust $ EventsPage.event page3 0
          retrievedMessage = fromJust $ EventsPage.event page3 1
          retrievedLeave = fromJust $ EventsPage.event page3 2

      -- Verify decoded events match originals
      Join.decode retrievedJoin `shouldBe` Just joinEvent
      Message.decode retrievedMessage `shouldBe` Just messageEvent
      Leave.decode retrievedLeave `shouldBe` Just leaveEvent

    it "should handle multiple participants in a chat" $ do
      let alice = 1
          bob = 2
          timestamp = 1700000000

          -- Alice joins
          aliceJoin = Join.Event alice timestamp "Alice"
          -- Bob joins
          bobJoin = Join.Event bob (timestamp + 1) "Bob"
          -- Alice sends message
          aliceMsg = Message.Event alice (timestamp + 2) "Hi Bob!"
          -- Bob sends message
          bobMsg = Message.Event bob (timestamp + 3) "Hi Alice!"
          -- Alice leaves
          aliceLeave = Leave.Event alice (timestamp + 4)
          -- Bob leaves
          bobLeave = Leave.Event bob (timestamp + 5)

      -- Add all events to page
      let page1 = fromJust $ EventsPage.addEvent EventsPage.emptyPage (Join.encode aliceJoin)
          page2 = fromJust $ EventsPage.addEvent page1 (Join.encode bobJoin)
          page3 = fromJust $ EventsPage.addEvent page2 (Message.encode aliceMsg)
          page4 = fromJust $ EventsPage.addEvent page3 (Message.encode bobMsg)
          page5 = fromJust $ EventsPage.addEvent page4 (Leave.encode aliceLeave)
          page6 = fromJust $ EventsPage.addEvent page5 (Leave.encode bobLeave)

      -- Verify all 6 events stored
      EventsPage.eventCount page6 `shouldBe` 6

      -- Verify each event decodes correctly
      Join.decode (fromJust $ EventsPage.event page6 0) `shouldBe` Just aliceJoin
      Join.decode (fromJust $ EventsPage.event page6 1) `shouldBe` Just bobJoin
      Message.decode (fromJust $ EventsPage.event page6 2) `shouldBe` Just aliceMsg
      Message.decode (fromJust $ EventsPage.event page6 3) `shouldBe` Just bobMsg
      Leave.decode (fromJust $ EventsPage.event page6 4) `shouldBe` Just aliceLeave
      Leave.decode (fromJust $ EventsPage.event page6 5) `shouldBe` Just bobLeave

    it "should correctly identify event types" $ do
      let joinEvent = Join.encode $ Join.Event 1 1700000000 "Test"
          messageEvent = Message.encode $ Message.Event 1 1700000001 "Hello"
          leaveEvent = Leave.encode $ Leave.Event 1 1700000002

      -- Verify event type bytes
      BS.index joinEvent 0 `shouldBe` 0x01
      BS.index messageEvent 0 `shouldBe` 0x03
      BS.index leaveEvent 0 `shouldBe` 0x02

    it "should handle page serialization with chat events" $ do
      let events =
            [ Join.encode $ Join.Event 1 1700000000 "User1"
            , Message.encode $ Message.Event 1 1700000001 "Message 1"
            , Message.encode $ Message.Event 1 1700000002 "Message 2"
            , Leave.encode $ Leave.Event 1 1700000003
            ]

      -- Add all events to page, using fromJust to fail test if any event fails to add
      let resultPage = foldl (\pg e -> fromJust $ EventsPage.addEvent pg e) EventsPage.emptyPage events

      -- Serialize and deserialize
      let serialized = EventsPage.toByteString resultPage
          deserialized = EventsPage.fromByteString serialized

      -- Verify round-trip
      deserialized `shouldBe` Right resultPage
      case deserialized of
        Right p -> EventsPage.eventCount p `shouldBe` 4
        Left _ -> expectationFailure "Failed to deserialize page"
