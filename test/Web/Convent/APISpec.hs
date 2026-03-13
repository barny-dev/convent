{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.APISpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)

import Web.Convent.Storage.ChatStore (newChatStore, ChatStoreConfig(..))
import qualified Web.Convent.Storage.ChatStore as ChatStore

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
        
        -- Verify files exist
        let indexPath = chatDir ++ "/index.dat"
            eventsPath = chatDir ++ "/events.dat"
        
        indexExists <- doesFileExist indexPath
        indexExists `shouldBe` True
        
        eventsExists <- doesFileExist eventsPath
        eventsExists `shouldBe` True
        
        indexContent <- BS.readFile indexPath
        BS.length indexContent `shouldBe` 8192
        
        eventsContent <- BS.readFile eventsPath
        BS.length eventsContent `shouldBe` 8192
    
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
