module Dissent.NetworkSpec where

import Network.Socket hiding (connect)
import Network.Simple.TCP (connect)

import Control.Concurrent (forkIO,
                           killThread,
                           threadDelay)
import Control.Concurrent.MVar

import qualified Dissent.Quorum  as Q(initialize)
import qualified Dissent.Network as N(quorumAcceptLoop)
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
          port   = "1234"
          quorum = fromRight (Q.initialize [T.Address addr port, T.Address addr port, T.Address addr port] (T.Address addr port))

      varAccept  <- (newEmptyMVar :: IO (MVar Int)) -- Use this var to detect whether the socket was accepted
      varConnect <- (newEmptyMVar :: IO (MVar Int)) -- Use this var to detect whether the socket was accepted

      threadId <- forkIO $ N.quorumAcceptLoop quorum (\_ -> putMVar varAccept 1)

      threadDelay 10000
      connect addr port (\_ -> putMVar varConnect 1)

      -- Blocks until the var is written to
      readMVar varAccept  `shouldReturn` 1
      readMVar varConnect `shouldReturn` 1

      killThread threadId
