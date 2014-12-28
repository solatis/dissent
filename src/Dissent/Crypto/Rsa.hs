-- | RSA crypto utility functions
module Dissent.Crypto.Rsa where

import Data.Maybe (fromJust)
import qualified Data.ByteString        as BS

import OpenSSL
import qualified OpenSSL.EVP.Cipher     as Cipher
import qualified OpenSSL.RSA            as RSA
import qualified OpenSSL.EVP.PKey       as PKey
import qualified OpenSSL.EVP.Seal       as Seal
import qualified OpenSSL.EVP.Open       as Open

import qualified Dissent.Internal.Debug as D

type PublicKey  = RSA.RSAPubKey
type PrivateKey = RSA.RSAKeyPair

data KeyPair = KeyPair {
  public  :: PublicKey,
  private :: PrivateKey
  }

data Encrypted = Encrypted {
  output :: BS.ByteString,  -- ^ Output string
  key    :: BS.ByteString,  -- ^ Encrypted assymetric key
  iv     :: BS.ByteString   -- ^ Input vector
  } deriving (Eq, Show)

-- | We will be using AES as Cipher for our RSA encryption
getCipher :: IO Cipher.Cipher
getCipher = withOpenSSL $ do
  (return . fromJust) =<< (Cipher.getCipherByName "AES-256-CBC")

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
encrypt publicKey input = withOpenSSL $ do
  cipher <- getCipher

  (encrypted, [encKey], inputVector) <- Seal.sealBS cipher [PKey.fromPublicKey publicKey] input

  return (Encrypted encrypted encKey inputVector)

decrypt :: PrivateKey -> Encrypted -> IO BS.ByteString
decrypt privateKey encrypted =

  let encKey      = key    encrypted
      inputVector = iv     encrypted
      input       = output encrypted

  in do
    cipher <- getCipher
    return (Open.openBS cipher encKey inputVector privateKey input)
