module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Web.Convent.API (API, server)
import Web.Convent.Storage.ChatStore (newChatStore, loadExistingChats, ChatStoreConfig(..))

main :: IO ()
main = do
  putStrLn "Starting Convent server on port 8080..."
  -- Create ChatStore with default configuration
  let config = ChatStoreConfig { chatsDirectory = "chats" }
  store <- newChatStore config
  
  -- Load existing chats from disk into memory
  result <- loadExistingChats store
  case result of
    Left err -> putStrLn $ "Warning: Failed to load existing chats: " ++ err
    Right count -> putStrLn $ "Loaded " ++ show count ++ " existing chats"
  
  -- Start server
  run 8080 (serve (Proxy :: Proxy API) (server store))
