{-# LANGUAGE DeriveGeneric #-}

-- | RSA crypto utility functions
--
--   High-level wrapper around the somewhat arcane HsOpenssl library. There are
--   various alternative libraries around, but I don't trust them. I would really
--   like to use libsodium / saltine, but the author specifically advices against
--   using it, so I will follow that advice.
--
--   Furthermore, there are various other pure-haskell RSA/AES libraries around,
--   but I don't think they have undergone the same scrutiny and peer reviews as
--   OpenSSL has. So for now, we will wrap OpenSSL in a nice interface, and mark
--   this as a to-do for the future. We will hard-code our preferences in this
--   module.
--
--   The implementation of this module, and usage of the HsOpenSSL API, will need
--   proper peer review.
module Dissent.Crypto.Rsa where

import           GHC.Generics           (Generic)

import qualified Data.Binary            as B
import qualified Data.ByteString        as BS
import           Data.Maybe             (fromJust)

import           OpenSSL                (withOpenSSL)
import qualified OpenSSL.EVP.Cipher     as Cipher
import qualified OpenSSL.EVP.Open       as Open
import qualified OpenSSL.EVP.PKey       as PKey
import qualified OpenSSL.EVP.Seal       as Seal
import qualified OpenSSL.PEM            as PEM
import qualified OpenSSL.RSA            as RSA


import qualified Crypto.Padding         as Pad (padPKCS5, unpadPKCS5)

import qualified Dissent.Internal.Debug as D

type PublicKey  = RSA.RSAPubKey
type PrivateKey = RSA.RSAKeyPair

data KeyPair = KeyPair {
  public  :: PublicKey,
  private :: PrivateKey
  } deriving (Eq, Show)

data Encrypted = Encrypted {
  output :: BS.ByteString,  -- ^ Output string
  key    :: BS.ByteString,  -- ^ Encrypted assymetric key
  iv     :: BS.ByteString   -- ^ Input vector
  } deriving (Eq, Show, Generic)

instance B.Binary Encrypted

-- Our cipher key length
cipherBits :: Int
cipherBits = 256

allCiphers :: IO [String]
allCiphers = withOpenSSL $ Cipher.getCipherNames

cipherName :: String
cipherName = "AES-" ++ show cipherBits ++ "-CBC"

-- When we are using a cypher in block mode, we need to ensure we are using a
-- proper padding, otherwise an attacker can determine the message length of
-- the last block.
--
-- It is tempting to think that paddingBytes = cypherBits / 8, but the
-- cypher bits is the *key* length, not the block length. AES uses
-- a fixed block length, according to Wikipedia:
--
-- "AES is a variant of Rijndael which has a fixed block size of 128
--  bits"
--
--  http://en.wikipedia.org/wiki/Advanced_Encryption_Standard
paddingBytes :: Int
paddingBytes = quot 128 8

-- | We will be using AES as Cipher for our RSA encryption
getCipher :: IO Cipher.Cipher
getCipher = withOpenSSL $ do
  (return . fromJust) =<< (Cipher.getCipherByName cipherName)

-- | We want to be able to represent our public key as string
serializePublicKey :: PublicKey -> IO String
serializePublicKey = PEM.writePublicKey

-- | And we want to be able to get our string representation back as public key
deserializePublicKey :: String -> IO PublicKey
deserializePublicKey someKey =
  let unsafeCast :: PKey.SomePublicKey -> PublicKey
      unsafeCast = fromJust . PKey.toPublicKey

  in (return . unsafeCast) =<< PEM.readPublicKey someKey

generateKeyPair :: IO KeyPair
generateKeyPair = withOpenSSL $
      -- Use a large key size / exponent
  let keySize          = 4096
      keyExponent      = 65537

      generateRsaKey   = RSA.generateRSAKey' keySize keyExponent

      -- Extracts public key part out of a private key
      extractPublicKey :: PrivateKey -> IO PublicKey
      extractPublicKey = RSA.rsaCopyPublic

  in do
    privateKey <- D.log ("Generating RSA key pair") (generateRsaKey)
    publicKey  <- extractPublicKey privateKey

    return (KeyPair publicKey privateKey)

encrypt :: PublicKey -> BS.ByteString -> IO Encrypted
encrypt publicKey input = withOpenSSL $
      -- AES-CBC requires us to pad the input message if the messages are/can be
      -- of variable length.
  let pad :: BS.ByteString -> BS.ByteString
      pad = Pad.padPKCS5 paddingBytes

  in do
    cipher <- getCipher

    (encrypted, [encKey], inputVector) <- Seal.sealBS cipher [PKey.fromPublicKey publicKey] (pad input)

    return (Encrypted encrypted encKey inputVector)

decrypt :: PrivateKey -> Encrypted -> IO BS.ByteString
decrypt privateKey encrypted =

  let encKey      = key    encrypted
      inputVector = iv     encrypted
      input       = output encrypted

      unpad :: BS.ByteString -> BS.ByteString
      unpad = Pad.unpadPKCS5

  in do
    cipher <- getCipher
    (return . unpad) (Open.openBS cipher encKey inputVector privateKey input)
