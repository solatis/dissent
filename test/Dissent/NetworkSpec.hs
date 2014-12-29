module Dissent.NetworkSpec where

import Data.Either (isRight)

import Control.Concurrent (forkIO,
                           killThread,
                           threadDelay)

import qualified Dissent.Quorum  as Q(initialize)
import qualified Dissent.Network as N
import qualified Dissent.Util    as U

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
          quorum = fromRight (Q.initialize [U.addressStub "0.0.0.0" port, U.addressStub addr port, U.addressStub "0.0.0.1" port] (U.addressStub addr port))

      threadId <- forkIO $ do
        socket <- N.quorumAcceptOne quorum
        putStrLn ("Accepted socket: " ++ show socket)
        return ()

      putStrLn "Created quorum that accepts connections.."

      threadDelay 100000
      socket <- N.connectSocket "127.0.0.1" 1234

      putStrLn "Connected to socket.."

      -- Blocks until the var is written to
      socket       `shouldSatisfy` isRight

      killThread threadId


    it "also accepts ipv6 connections" $ do

      -- Notice how our own address is "127.0.0.1" ..
      let addr   = "127.0.0.1"
          port   = 1235
          quorum = fromRight (Q.initialize [U.addressStub "0.0.0.0" port, U.addressStub addr port, U.addressStub "0.0.0.1" port] (U.addressStub addr port))

      threadId <- forkIO $ do
        socket <- N.quorumAcceptOne quorum
        putStrLn ("Accepted socket: " ++ show socket)
        return ()

      putStrLn "Created quorum that accepts connections.."

      threadDelay 100000

      -- .. but we connect to ::1
      socket <- N.connectSocket "::1" 1235

      putStrLn "Connected to socket.."

      -- Blocks until the var is written to
      socket       `shouldSatisfy` isRight

      killThread threadId

  describe "connecting to another node in the quorum" $ do
    it "fails when the node is not available" $ do

      let firstAddress  = U.addressStub "127.0.0.1" 1236
          secondAddress = U.addressStub "127.0.0.1" 1237
          addresses     = [firstAddress, secondAddress]

          quorum   = fromRight (Q.initialize addresses firstAddress)

      result <- N.quorumConnect' quorum (N.Attempts 1) 1000000
      result `shouldBe` Left "Unable to connect to remote"

    it "succeeds when the node is available" $ do
      let firstAddress  = U.addressStub "127.0.0.1" 1238
          secondAddress = U.addressStub "127.0.0.1" 1239
          addresses     = [firstAddress, secondAddress]

          firstQuorum   = fromRight (Q.initialize addresses firstAddress)
          secondQuorum  = fromRight (Q.initialize addresses secondAddress)

      thread <- forkIO $ do
                _ <- N.quorumAcceptOne firstQuorum
                return ()
      result <- N.quorumConnect secondQuorum

      isRight (result) `shouldBe` True

      killThread thread


    it "fails when trying to connect to the same node multiple times" $ do
      let firstAddress  = U.addressStub "127.0.0.1" 1240
          secondAddress = U.addressStub "127.0.0.1" 1241
          addresses     = [firstAddress, secondAddress]

          firstQuorum   = fromRight (Q.initialize addresses firstAddress)
          secondQuorum  = fromRight (Q.initialize addresses secondAddress)

      thread <- forkIO $ do
                _ <- N.quorumAcceptOne firstQuorum
                return ()

      _ <- N.quorumConnect secondQuorum
      result <- N.quorumConnect' secondQuorum (N.Attempts 10) 100000
      result `shouldBe` Left "Unable to connect to remote"

      killThread thread
