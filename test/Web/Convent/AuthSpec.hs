{-# LANGUAGE OverloadedStrings #-}

module Web.Convent.AuthSpec (spec) where

import Test.Hspec

import qualified Web.Convent.Auth as Auth

spec :: Spec
spec = describe "Auth" $ do
  it "registers a new user" $ do
    store <- Auth.newAuthStore
    result <- Auth.registerUser store "alice" "password-123"
    result `shouldBe` Right ()

  it "rejects duplicate users" $ do
    store <- Auth.newAuthStore
    _ <- Auth.registerUser store "alice" "password-123"
    result <- Auth.registerUser store "alice" "different"
    result `shouldBe` Left "User already exists"

  it "exchanges valid password for token and authenticates it" $ do
    store <- Auth.newAuthStore
    _ <- Auth.registerUser store "alice" "password-123"
    tokenResult <- Auth.exchangePasswordForToken store "alice" "password-123"
    case tokenResult of
      Left err -> expectationFailure err
      Right token -> do
        token `shouldNotBe` ""
        authenticatedUser <- Auth.authenticateToken store token
        authenticatedUser `shouldBe` Just "alice"

  it "rejects invalid password exchange" $ do
    store <- Auth.newAuthStore
    _ <- Auth.registerUser store "alice" "password-123"
    tokenResult <- Auth.exchangePasswordForToken store "alice" "wrong-password"
    tokenResult `shouldBe` Left "Invalid username or password"
