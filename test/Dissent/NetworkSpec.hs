module Dissent.NetworkSpec where

import Network.Simple.TCP (connect)
import Data.Either (isRight)

import Control.Concurrent (forkIO,
                           killThread,
                           threadDelay)
import Control.Concurrent.MVar

import qualified Dissent.Quorum  as Q(initialize)
import qualified Dissent.Network as N
import qualified Dissent.Types   as T

import Test.Hspec

fromRight :: Either a b -> b
fromRight e =
  case e of
   Right r -> r

spec :: Spec
spec = do
  describe "launching the quorum accept loop" $ do
    it "listens on the correct address" $ do

      let addr   = "127.0.0.1"
          port   = 1234
          quorum = fromRight (Q.initialize [T.Address "0.0.0.0" port, T.Address addr port, T.Address "0.0.0.1" port] (T.Address addr port))

      varAccept  <- newEmptyMVar
      varConnect <- newEmptyMVar

      threadId <- forkIO $ putMVar varAccept =<< N.quorumAccept quorum

      threadDelay 100000
      connect addr (show port) (\_ -> putMVar varConnect 1)

      -- Blocks until the var is written to
      isEmptyMVar varAccept  `shouldReturn` False
      readMVar    varConnect `shouldReturn` (1 :: Integer)

      killThread threadId

  describe "connecting to another node in the quorum" $ do
    it "fails when the node is not available" $ do

      let firstAddress  = T.Address "127.0.0.1" 1234
          secondAddress = T.Address "127.0.0.1" 1235
          addresses     = [firstAddress, secondAddress]

          quorum   = fromRight (Q.initialize addresses firstAddress)

      result <- N.quorumConnect' quorum (N.Attempts 1) 100000
      result `shouldBe` Left "Unable to connect to remote"

    it "succeeds when the node is available" $ do
      let firstAddress  = T.Address "127.0.0.1" 1234
          secondAddress = T.Address "127.0.0.1" 1235
          addresses     = [firstAddress, secondAddress]

          firstQuorum   = fromRight (Q.initialize addresses firstAddress)
          secondQuorum  = fromRight (Q.initialize addresses secondAddress)

      thread <- forkIO $ do
                _ <- N.quorumAccept firstQuorum
                return ()
      result <- N.quorumConnect secondQuorum

      isRight (result) `shouldBe` True

      killThread thread


    it "fails when trying to connect to the same node multiple times" $ do
      let firstAddress  = T.Address "127.0.0.1" 1234
          secondAddress = T.Address "127.0.0.1" 1235
          addresses     = [firstAddress, secondAddress]

          firstQuorum   = fromRight (Q.initialize addresses firstAddress)
          secondQuorum  = fromRight (Q.initialize addresses secondAddress)

      thread <- forkIO $ do
                _ <- N.quorumAccept firstQuorum
                return ()

      _ <- N.quorumConnect secondQuorum
      result <- N.quorumConnect' secondQuorum (N.Attempts 10) 100000
      result `shouldBe` Left "Unable to connect to remote"

      killThread thread
