{-# LANGUAGE OverloadedStrings #-}

module Dissent.Crypto.RsaSpec where

import Dissent.Crypto.Rsa

import Test.Hspec

spec :: Spec
spec = do
  describe "generating a public/private key pair" $ do
    it "should be able to encrypt data" $ do
      let secret = "Hello, world!"

      pair      <- generateKeyPair
      encrypted <- encrypt (public pair) secret

      (output encrypted) `shouldSatisfy` (/= secret)

    it "should be able to encrypt and decrypt data" $ do
      let secret = "Hello, world!"

      pair      <- generateKeyPair
      encrypted <- encrypt (public pair) secret
      decrypted <- decrypt (private pair) encrypted

      decrypted `shouldBe` secret
