module Dissent.NetworkSpec where

import           Data.Either                  (isRight)

import           Control.Concurrent           (threadDelay)

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (resourceForkIO, runResourceT)


import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Network.Socket       as NS
import qualified Dissent.Quorum               as Q (initialize)
import qualified Dissent.Types                as T
import qualified Dissent.Util                 as U

import           Test.Hspec

spec :: Spec
spec = do
  describe "launching the quorum accept loop" $ do

    it "listens on the correct address" $ runResourceT $ do

      let addr   = "127.0.0.1"
          port   = 1234
          quorum = U.fromRight (Q.initialize [U.remoteStub "0.0.0.0" port, U.remoteStub addr port, U.remoteStub "0.0.0.1" port] (U.remoteStub addr port))

      _ <- resourceForkIO $ do
        [socket] <- NQ.accept quorum T.Slave
        liftIO $ putStrLn ("Accepted socket: " ++ show socket)

      liftIO $ do
         putStrLn "Created quorum that accepts connections.."
         threadDelay 100000

      socket <- NS.connect "127.0.0.1" 1234

      liftIO $ do
        putStrLn "Connected to socket.."
        socket `shouldSatisfy` isRight

    it "also accepts ipv6 connections" $ runResourceT $ do

      -- Notice how our own address is "127.0.0.1" ..
      let addr   = "127.0.0.1"
          port   = 1235
          quorum = U.fromRight (Q.initialize [U.remoteStub "0.0.0.0" port, U.remoteStub addr port, U.remoteStub "0.0.0.1" port] (U.remoteStub addr port))

      _ <- resourceForkIO $ do
        [socket] <- NQ.accept quorum T.Slave
        liftIO $ putStrLn ("Accepted socket: " ++ show socket)

      liftIO $ do
        putStrLn "Created quorum that accepts connections.."
        threadDelay 100000

      -- .. but we connect to ::1
      socket <- NS.connect "::1" 1235

      liftIO $ do
        putStrLn "Connected to socket.."
        socket `shouldSatisfy` isRight

  describe "connecting to another node in the quorum" $ do
    it "fails when the node is not available" $ runResourceT $ do

      let firstAddress  = U.remoteStub "127.0.0.1" 1236
          secondAddress = U.remoteStub "127.0.0.1" 1237
          addresses     = [firstAddress, secondAddress]

          quorum   = U.fromRight (Q.initialize addresses firstAddress)

      result <- NQ.connect quorum T.Slave (NQ.Attempts 1)
      liftIO $ (result `shouldBe` Left "Unable to connect to remote")

    it "succeeds when the node is available" $ runResourceT $ do
      let firstAddress  = U.remoteStub "127.0.0.1" 1238
          secondAddress = U.remoteStub "127.0.0.1" 1239
          addresses     = [firstAddress, secondAddress]

          firstQuorum   = U.fromRight (Q.initialize addresses firstAddress)
          secondQuorum  = U.fromRight (Q.initialize addresses secondAddress)

      _ <- resourceForkIO $ do
        [socket] <- NQ.accept firstQuorum T.Slave
        liftIO $ putStrLn ("Accepted socket: " ++ show socket)

      result <- NQ.connect secondQuorum T.Slave (NQ.Infinity)

      liftIO $ do
        isRight (result) `shouldBe` True
