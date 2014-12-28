{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Dissent.Crypto.Rsa as R

import qualified Data.ByteString as BS

recrypt :: R.KeyPair -> BS.ByteString -> IO BS.ByteString
recrypt pair secret = do
  encrypted <- R.encrypt (R.public pair) secret
  R.decrypt (R.private pair) encrypted


encrypt :: R.KeyPair -> BS.ByteString -> IO R.Encrypted
encrypt pair secret = do
  R.encrypt (R.public pair) secret

generateBS :: Int -> BS.ByteString
generateBS count =
  BS.replicate count 3 -- randomly chosen

main :: IO ()
main = do
  pair      <- R.generateKeyPair

  let kb1   = (generateBS 1024)
      mb1   = (generateBS 1048576)
      mb8   = (generateBS 8388608)
      mb16  = (generateBS 16777216)
      mb32  = (generateBS 33554432)
      mb64  = (generateBS 67108864)

  defaultMain
    [ bgroup "rsa/encrypt" [ bench "1kb"     $ whnfIO (encrypt pair kb1)
                           , bench "1mb"     $ whnfIO (encrypt pair mb1)
                           , bench "8mb"     $ whnfIO (encrypt pair mb8)
                           , bench "16mb"    $ whnfIO (encrypt pair mb16)
                           , bench "32mb"    $ whnfIO (encrypt pair mb32)
                           , bench "64mb"    $ whnfIO (encrypt pair mb64)
                           ]
    , bgroup "rsa/recrypt" [ bench "1kb"     $ whnfIO (recrypt pair kb1)
                           , bench "1mb"     $ whnfIO (recrypt pair mb1)
                           , bench "8mb"     $ whnfIO (recrypt pair mb8)
                           , bench "16mb"    $ whnfIO (recrypt pair mb16)
                           , bench "32mb"    $ whnfIO (recrypt pair mb32)
                           , bench "64mb"    $ whnfIO (recrypt pair mb64)
                           ]

    ]
