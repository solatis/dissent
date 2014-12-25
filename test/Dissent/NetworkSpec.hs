module Dissent.NetworkSpec where

import Network.Socket
import Network.BSD (getProtocolNumber)

import Control.Concurrent (forkIO,
                           killThread,
                           threadDelay)
import Control.Concurrent.MVar

import qualified Dissent.Quorum  as Q(initialize)
import qualified Dissent.Network as N(acceptLoop,
                                      quorumAcceptLoop)
import qualified Dissent.Types   as T
import qualified Data.Vector     as V

import Test.Hspec

fromRight :: Either a b -> b
fromRight e =
  case e of
   Right r -> r

spec :: Spec
spec = do
  describe "launching the accept loop" $ do
    it "allows us to connect to the socket" $ do
      let port = 1234

      var      <- (newEmptyMVar :: IO (MVar Int)) -- Use this var to detect whether the socket was connected to
      threadId <- forkIO $ N.acceptLoop "127.0.0.1" port (\_ -> putMVar var 1)
      proto    <- getProtocolNumber "tcp"
      sock     <- socket AF_INET Stream proto
      addr     <- inet_addr "127.0.0.1"

      threadDelay 10000
      connect sock (SockAddrInet port addr)

      -- Blocks until the var is written to
      readMVar var `shouldReturn` 1

      killThread threadId

  describe "launching the quorum accept loop" $ do
    it "listens on the correct address" $ do

      addr     <- inet_addr "127.0.0.1"

      let port   = 1235
          quorum = fromRight (Q.initialize [SockAddrInet port 0, SockAddrInet port addr, SockAddrInet port 1] (SockAddrInet port addr))

      var      <- (newEmptyMVar :: IO (MVar Int)) -- Use this var to detect whether the socket was connected to
      threadId <- forkIO $ N.quorumAcceptLoop quorum (\_ -> putMVar var 1)
      proto    <- getProtocolNumber "tcp"
      sock     <- socket AF_INET Stream proto
      addr     <- inet_addr "127.0.0.1"

      threadDelay 10000
      connect sock (SockAddrInet port addr)

      -- Blocks until the var is written to
      readMVar var `shouldReturn` 1

      killThread threadId
