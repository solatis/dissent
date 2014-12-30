module Dissent.NetworkSpec where

import Data.Either (isRight)

import Control.Concurrent (threadDelay)

import Control.Monad.Trans.Resource ( runResourceT
                                    , resourceForkIO)
import Control.Monad.IO.Class (liftIO)


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

    it "listens on the correct address" $ runResourceT $ do

      let addr   = "127.0.0.1"
          port   = 1234
          quorum = fromRight (Q.initialize [U.addressStub "0.0.0.0" port, U.addressStub addr port, U.addressStub "0.0.0.1" port] (U.addressStub addr port))

      _ <- resourceForkIO $ do
        socket <- N.quorumAcceptOne quorum
        liftIO $ putStrLn ("Accepted socket: " ++ show socket)

      liftIO $ do
         putStrLn "Created quorum that accepts connections.."
         threadDelay 100000

      socket <- N.connectSocket "127.0.0.1" 1234

      liftIO $ do
        putStrLn "Connected to socket.."
        socket `shouldSatisfy` isRight

    it "also accepts ipv6 connections" $ runResourceT $ do

      -- Notice how our own address is "127.0.0.1" ..
      let addr   = "127.0.0.1"
          port   = 1235
          quorum = fromRight (Q.initialize [U.addressStub "0.0.0.0" port, U.addressStub addr port, U.addressStub "0.0.0.1" port] (U.addressStub addr port))

      _ <- resourceForkIO $ do
        socket <- N.quorumAcceptOne quorum
        liftIO $ putStrLn ("Accepted socket: " ++ show socket)

      liftIO $ do
        putStrLn "Created quorum that accepts connections.."
        threadDelay 100000

      -- .. but we connect to ::1
      socket <- N.connectSocket "::1" 1235

      liftIO $ do
        putStrLn "Connected to socket.."
        socket `shouldSatisfy` isRight

  describe "connecting to another node in the quorum" $ do
    it "fails when the node is not available" $ runResourceT $ do

      let firstAddress  = U.addressStub "127.0.0.1" 1236
          secondAddress = U.addressStub "127.0.0.1" 1237
          addresses     = [firstAddress, secondAddress]

          quorum   = fromRight (Q.initialize addresses firstAddress)

      result <- N.quorumConnect' quorum (N.Attempts 1) 1000000
      liftIO $ (result `shouldBe` Left "Unable to connect to remote")

    it "succeeds when the node is available" $ runResourceT $ do
      let firstAddress  = U.addressStub "127.0.0.1" 1238
          secondAddress = U.addressStub "127.0.0.1" 1239
          addresses     = [firstAddress, secondAddress]

          firstQuorum   = fromRight (Q.initialize addresses firstAddress)
          secondQuorum  = fromRight (Q.initialize addresses secondAddress)

      _ <- resourceForkIO $ do
        socket <- N.quorumAcceptOne firstQuorum
        liftIO $ putStrLn ("Accepted socket: " ++ show socket)

      result <- N.quorumConnect secondQuorum

      liftIO $ do
        isRight (result) `shouldBe` True
