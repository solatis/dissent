module Dissent.Protocol.Shuffle.Leader where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource

import           Data.Either                  ()
import qualified Network.Socket               as NS

import qualified Dissent.Crypto.Rsa           as R
import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Network.Socket       as NS
import qualified Dissent.Types                as T

run :: T.Quorum -> IO ()
run quorum = runResourceT $ do
  sockets <- phase1 quorum
  _ <- liftIO $ phase2 sockets

  return ()

-- | In the first phase, our leader will open a socket that
--   slaves can connect to, and will wait for all slaves to
--   connect to the quorum.
--
--   This is a blocking operation.
phase1 :: T.Quorum                 -- ^ The Quorum we operate on
       -> ResourceT IO [NS.Socket] -- ^ The sockets we accepted
phase1 quorum = (return . map fst) =<< (NQ.accept quorum T.Leader)

-- | In the second phase, the leader receives all the encrypted messages from all
--   the slaves.
--
--   This is a blocking operation.
phase2 :: [NS.Socket]                     -- ^ The Sockets we accepted
       -> IO [Either String R.Encrypted]  -- ^ All the encrypted messages we received from the
                                          --   slaves.
phase2 sockets = mapM (NS.receiveAndDecode) sockets
