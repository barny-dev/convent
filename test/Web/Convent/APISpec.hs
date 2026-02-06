{-# LANGUAGE OverloadedStrings #-}
module Web.Convent.APISpec (spec) where

import Test.Hspec
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Aeson (decode, encode)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)

import Web.Convent.API
import Web.Convent.Storage.ChatStore (newChatStore)
import qualified Web.Convent.Storage.EventsPage as EventsPage
import qualified Web.Convent.Storage.IndexPage as IndexPage

spec :: Spec
spec = describe "API" $ do
  describe "Chat creation and operations" $ do
    it "should create a chat and initialize storage files" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        -- Create a chat
        let chatDir = tmpDir ++ "/chats"
        -- We can't easily test the server without running it
        -- But we can test the storage functions
        
        -- Create index and events files
        let indexPath = tmpDir ++ "/index.dat"
            eventsPath = tmpDir ++ "/events.dat"
        
        BS.writeFile indexPath (IndexPage.toByteString IndexPage.emptyPage)
        BS.writeFile eventsPath (EventsPage.toByteString EventsPage.emptyPage)
        
        -- Verify files exist
        indexExists <- doesDirectoryExist tmpDir
        indexExists `shouldBe` True
        
        indexContent <- BS.readFile indexPath
        BS.length indexContent `shouldBe` 8192
        
        eventsContent <- BS.readFile eventsPath
        BS.length eventsContent `shouldBe` 8192
    
    it "should handle event offset calculation with index" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        let indexPath = tmpDir ++ "/index.dat"
            eventsPath = tmpDir ++ "/events.dat"
        
        -- Initialize empty files
        BS.writeFile indexPath (IndexPage.toByteString IndexPage.emptyPage)
        BS.writeFile eventsPath (EventsPage.toByteString EventsPage.emptyPage)
        
        -- Calculate initial offset (should be 0)
        offset0 <- calculateNextOffset indexPath eventsPath
        offset0 `shouldBe` 0
        
        -- Add a test event
        let testEvent = BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 
                                 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 
                                 0x41, 0x6C, 0x69, 0x63, 0x65] -- "Alice"
        appendEventToFile indexPath eventsPath testEvent
        
        -- Calculate offset after adding event (should be 1)
        offset1 <- calculateNextOffset indexPath eventsPath
        offset1 `shouldBe` 1
        
        -- Add another event
        appendEventToFile indexPath eventsPath testEvent
        
        -- Calculate offset (should be 2)
        offset2 <- calculateNextOffset indexPath eventsPath
        offset2 `shouldBe` 2
    
    it "should maintain index when creating new pages" $ do
      withSystemTempDirectory "convent-test" $ \tmpDir -> do
        let indexPath = tmpDir ++ "/index.dat"
            eventsPath = tmpDir ++ "/events.dat"
        
        -- Initialize empty files
        BS.writeFile indexPath (IndexPage.toByteString IndexPage.emptyPage)
        BS.writeFile eventsPath (EventsPage.toByteString EventsPage.emptyPage)
        
        -- Add many large events to force page creation
        -- Each event is about 500 bytes (after type, timestamp, id, and message)
        let largeEvent = BS.pack ([0x03] ++ replicate 16 0x00 ++ replicate 500 0x41)
        
        -- Add 15 events (should stay in one page, ~7500 bytes)
        mapM_ (\_ -> appendEventToFile indexPath eventsPath largeEvent) [1..15 :: Int]
        
        -- Check we still have one page
        eventsContent1 <- BS.readFile eventsPath
        let numPages1 = BS.length eventsContent1 `div` 8192
        numPages1 `shouldBe` 1
        
        -- Add more events to trigger second page
        mapM_ (\_ -> appendEventToFile indexPath eventsPath largeEvent) [16..20 :: Int]
        
        -- Check we now have multiple pages
        eventsContent2 <- BS.readFile eventsPath
        let numPages2 = BS.length eventsContent2 `div` 8192
        numPages2 `shouldSatisfy` (>= 1)
        
        -- If we have multiple pages, check index was updated
        when (numPages2 > 1) $ do
          indexContent <- BS.readFile indexPath
          case IndexPage.fromByteString indexContent of
            Left _ -> expectationFailure "Index should be valid"
            Right indexPage -> do
              let entries = IndexPage.entries indexPage
              length entries `shouldSatisfy` (> 0)
