module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Web.Convent.API (API, server)
import Web.Convent.Storage.ChatStore (newChatStore)

main :: IO ()
main = do
  putStrLn "Starting Convent server on port 8080..."
  store <- newChatStore
  run 8080 (serve (Proxy :: Proxy API) (server store))
