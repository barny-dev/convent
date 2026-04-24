{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.APISpec (spec) where

import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Exception (finally)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)

import Web.Convent.Storage.ChatStore (newChatStore, ChatStoreConfig(..))
import qualified Web.Convent.Storage.ChatStore as ChatStore
import Web.Convent.API (waitForEvents)

spec :: Spec
spec = describe "API" $ do
  describe "Chat creation and operations" $ do
    it "should create a chat and initialize storage files" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        -- Create a chat store with test directory
        let config = ChatStoreConfig { ChatStore.chatsDirectory = tmpDir ++ "/chats" }
        store <- newChatStore config
        
        -- Create a chat
        uuid <- ChatStore.createChat store
        
        -- Verify directory was created
        let chatDir = tmpDir ++ "/chats/" ++ show uuid
        dirExists <- doesDirectoryExist chatDir
        dirExists `shouldBe` True
        
        -- Verify files exist (files are locked by ChatStore handles, so we can't read them)
        let indexPath = chatDir ++ "/index.dat"
            eventsPath = chatDir ++ "/events.dat"
        
        indexExists <- doesFileExist indexPath
        indexExists `shouldBe` True
        
        eventsExists <- doesFileExist eventsPath
        eventsExists `shouldBe` True
        
        -- Note: Cannot read file contents as files are now locked by ChatStore handles
        -- This is the correct behavior - active chats keep handles open for performance
    
    it "should handle join and post operations via ChatStore" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        -- Create a chat store with test directory
        let config = ChatStoreConfig { ChatStore.chatsDirectory = tmpDir ++ "/chats" }
        store <- newChatStore config
        
        -- Create a chat
        uuid <- ChatStore.createChat store
        
        -- Join chat
        joinResult <- ChatStore.joinChatParticipant store uuid "Alice"
        case joinResult of
          Left err -> expectationFailure $ "Join failed: " ++ err
          Right (pid, offset) -> do
            pid `shouldSatisfy` (> 0)
            offset `shouldBe` 0
        
        -- Post message
        let participantId = case joinResult of
              Right (pid, _) -> pid
              _ -> 0
        postResult <- ChatStore.postChatMessage store uuid participantId "Hello"
        case postResult of
          Left err -> expectationFailure $ "Post failed: " ++ err
          Right offset -> offset `shouldBe` 1
        
        -- Get events
        eventsResult <- ChatStore.getChatEvents store uuid 0
        case eventsResult of
          Left err -> expectationFailure $ "Get events failed: " ++ err
          Right evts -> length evts `shouldBe` 2

    it "should persist many posted messages across events pages" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        let config = ChatStoreConfig { ChatStore.chatsDirectory = tmpDir ++ "/chats" }
            messageCount = 1500  -- Large enough to span multiple 8KB events pages
            expectedJoinEvents = 1
        store <- newChatStore config

        uuid <- ChatStore.createChat store

        joinResult <- ChatStore.joinChatParticipant store uuid "Alice"
        case joinResult of
          Left err -> expectationFailure $ "Join failed: " ++ err
          Right (participantId, offset) -> do
            offset `shouldBe` 0

            mapM_ (\ix -> do
              postResult <- ChatStore.postChatMessage store uuid participantId (Text.pack ("Hello " ++ show ix))
              case postResult of
                Left postErr -> expectationFailure $ "Post failed at message " ++ show ix ++ ": " ++ postErr
                Right eventOffset -> eventOffset `shouldBe` fromIntegral ix
              ) [1 .. messageCount]

            eventsResult <- ChatStore.getChatEvents store uuid 0
            case eventsResult of
              Left getErr -> expectationFailure $ "Get events failed: " ++ getErr
              Right evts -> length evts `shouldBe` messageCount + expectedJoinEvents
    
    it "should handle chat existence check" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        -- Create a chat store with test directory
        let config = ChatStoreConfig { ChatStore.chatsDirectory = tmpDir ++ "/chats" }
        store <- newChatStore config
        
        -- Create a chat
        uuid <- ChatStore.createChat store
        
        -- Check it exists
        exists <- ChatStore.chatExists store uuid
        exists `shouldBe` True
        
        -- Check non-existent chat
        let fakeUuid = fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
        notExists <- ChatStore.chatExists store fakeUuid
        notExists `shouldBe` False

    it "should wait for new events via stream helper and return when available" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        let config = ChatStoreConfig { ChatStore.chatsDirectory = tmpDir ++ "/chats" }
        store <- newChatStore config
        uuid <- ChatStore.createChat store

        joinResult <- ChatStore.joinChatParticipant store uuid "Alice"
        participantId <- case joinResult of
          Left err -> expectationFailure ("Join failed: " ++ err) >> return 0
          Right (pid, _) -> return pid

        done <- newEmptyMVar
        _ <- forkIO $
          (do
            threadDelay 200000
            void (ChatStore.postChatMessage store uuid participantId "hello from stream"))
          `finally` putMVar done ()

        result <- waitForEvents store uuid 1 3000
        case result of
          Left err -> expectationFailure $ "waitForEvents failed: " ++ err
          Right evts -> length evts `shouldBe` 1
        takeMVar done

    it "should return empty event list when stream helper times out" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        let config = ChatStoreConfig { ChatStore.chatsDirectory = tmpDir ++ "/chats" }
        store <- newChatStore config
        uuid <- ChatStore.createChat store

        _ <- ChatStore.joinChatParticipant store uuid "Alice"

        result <- waitForEvents store uuid 1 50
        case result of
          Left err -> expectationFailure $ "waitForEvents failed: " ++ err
          Right evts -> evts `shouldBe` []
