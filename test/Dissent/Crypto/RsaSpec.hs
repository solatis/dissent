{-# LANGUAGE OverloadedStrings #-}

module Dissent.Crypto.RsaSpec where

import Data.ByteString as BS hiding (putStrLn,
                                     elem)
import Dissent.Crypto.Rsa

import Test.Hspec

recrypt :: BS.ByteString -> IO BS.ByteString
recrypt secret = do
  pair      <- generateKeyPair

  encrypted <- encrypt (public pair)  secret
  decrypt (private pair) encrypted

spec :: Spec
spec = do
  describe "our encryption cipher" $ do
    it "uses a 256 bit block cipher" $
      cipherBits `shouldBe` 256

    it "uses AES-256-CBC" $
      cipherName `shouldBe` "AES-256-CBC"

    it "is supported by our OS" $ do
      ciphers <- allCiphers
      elem cipherName ciphers `shouldBe` True

    it "uses 16 bytes padding" $
      paddingBytes `shouldBe` 16

  describe "generating a public/private key pair" $ do
    it "can serialize and deserialize a public key" $ do
      pair          <- generateKeyPair
      let publicKey = public pair

      serialized    <- serialize publicKey
      deserialized  <- deserialize (serialized)

      publicKey `shouldBe` deserialized

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
      let secret = BS.empty
          test   = do
            recrypted <-(recrypt secret)
            (show recrypted) `shouldBe` (show secret)

      in test
