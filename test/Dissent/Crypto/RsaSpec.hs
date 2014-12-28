{-# LANGUAGE OverloadedStrings #-}

module Dissent.Crypto.RsaSpec where

import Data.ByteString.Lazy.Char8 as BL
import Dissent.Crypto.Rsa

import Test.Hspec

recrypt :: BL.ByteString -> IO BL.ByteString
recrypt secret = do
  pair      <- generateKeyPair

  encrypted <- encrypt (public pair)  secret
  decrypt (private pair) encrypted

spec :: Spec
spec = do
  describe "generating a public/private key pair" $ do
    it "should be able to encrypt data" $ do
      let secret = "Hello, world!"

      pair      <- generateKeyPair
      encrypted <- encrypt (public pair) secret

      (output encrypted) `shouldSatisfy` (/= secret)

    it "should be able to encrypt and decrypt data" $
      let secret = "Hello, world!"
          test   = (recrypt secret) `shouldReturn` secret

      in test

    it "should be able to encrypt and decrypt empty data" $
      let secret = BL.empty
          test   = do
            recrypted <-(recrypt secret)
            (show recrypted) `shouldBe` (show secret)

      in test
