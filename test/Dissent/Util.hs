-- | Helper functions for Dissent test cases
module Dissent.Util where

import qualified Dissent.Crypto.Rsa as R
import           Network.Socket     (HostName, PortNumber)
import           System.IO.Unsafe   (unsafePerformIO)

import           System.Random


import qualified Dissent.Types.Remote      as TR

fromRight :: Either a b -> b
fromRight (Right r) = r
fromRight (Left  _) = error ("Not a right!")

randomString :: IO String
randomString = do
  gen <- newStdGen

  return (take 10 $ randomRs ('a','z') gen)

randomList :: Int -> IO [Int]
randomList l = do
  gen <- newStdGen

  return (take l $ randomRs (0,9) gen)

remoteStub :: HostName -> PortNumber -> TR.Remote
remoteStub h p =
  let serializedPkey = unlines ["-----BEGIN PUBLIC KEY-----",
                                "MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEArcrRDTxwvdhrgGDFd9zn",
                                "gmBC/t1/FI+EfHtxjSUk0p7UhqnI/wGHekaYuykAUEMxb7FC0K4Gl9EQvHlHHUlC",
                                "LldcbhA+PzTxcbNni7xUIsw8ZX8/gwC4VWA3WCiDO25/5nYua5LaESWHLF2iOHg3",
                                "AVbLCKXQNDbe14ekRPGqGvKCemSVW0TCb6HexwN1jAUNsXOYq2WZff4ShvVhkkTJ",
                                "4VQEShGO8BiYBk4AukEc2wYAjsT7ktQk4wUWN7QC1nfMXGmAm+WqnQu9viz6J/sp",
                                "sryR8tT/Wb/2F/qC1qaeRYgBCjZSWFeEr6PfzwkciGswPpZQQahorYQWp/vWsZkh",
                                "xR5bUi1giB0AwDszlLsqYI0TDhIj0qbfn5EI1zJsj+Er7+HdG/S/pO+mIY1Au81c",
                                "97+4GMDmojrpLav8d2+/pZbQOxCWmyHR/1uk2kWh6iRaQcalUF9GgXcfYLh8oc6v",
                                "lARLqdAJPx3mErPGMlKZYz+VieQK+yhFdHnwv/bztf6LMMniz+Q03OyHGEJyZ/P2",
                                "g5QAY4UJ9wN2C46Q1A/LPBuzIM/NGNpcDOJvAIxbuyjAIlW6hvnLBiGNWGLNDcQg",
                                "XmULSyUXNXy09xAvtfLqyPTfztNS/az3n1WlOPMuzteg65Lk99hU+J02voPLkuWM",
                                "YWLawiijIgC03msHbs0rg8UCAwEAAQ==",
                                "-----END PUBLIC KEY-----"]
      generateKey = unsafePerformIO $ R.deserializePublicKey (serializedPkey)

  in TR.Remote h p generateKey generateKey
