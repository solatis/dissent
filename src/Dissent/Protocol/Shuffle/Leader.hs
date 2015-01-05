module Dissent.Protocol.Shuffle.Leader where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Morph
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource

import           Data.List                    (sortBy)

import qualified Network.Socket               as NS

import qualified Dissent.Crypto.Rsa           as R
import qualified Dissent.Network.Quorum       as NQ
import qualified Dissent.Network.Socket       as NS
import qualified Dissent.Types                as T

instance MFunctor (ExceptT e) where
    hoist nat m = ExceptT (nat (runExceptT m))

run :: T.Quorum -> IO ()
run quorum = runResourceT $ do

  result <- runExceptT $ do
    sockets <- phase1 quorum
    ciphers <- hoist liftIO (phase2 sockets)
    _       <- hoist liftIO (phase3 sockets ciphers)
    return ()

  case result of
   Left  e -> error ("Something went wrong: " ++ e)
   Right b -> return (b)

-- | In the first phase, our leader will open a socket that
--   slaves can connect to, and will wait for all slaves to
--   connect to the quorum.
--
--   This is a blocking operation.
phase1 :: T.Quorum                                  -- ^ The Quorum we operate on
       -> ExceptT String (ResourceT IO) [NS.Socket] -- ^ The sockets we accepted
phase1 quorum =

  let accepted = NQ.accept quorum T.Leader

      -- Returns all accepted sockets from all slaves.
      --
      -- This is a blocking operation.
      sockets  :: ResourceT IO [NS.Socket]
      sockets  = (return . map fst) =<< accepted

      -- After a connection has been established with a slave, we
      -- expect a handshake to occur. At the present moment, this
      -- handshake only involves a peer telling us his id, so we know
      -- which socket to associate with which peer.
      --
      -- This is a blocking operation.
      handShake :: NS.Socket -> ExceptT String (ResourceT IO) T.PeerId
      handShake socket = do
        peerId <- liftIO $ NS.receiveAndDecode socket
        either throwE return peerId

      -- Now, after this process, we have a list of sockets, and a list
      -- of peer ids. Once we put them in a zipped list, we have a convenient
      -- way to sort them by peer id, thus allowing us to easily look up a
      -- socket by a peer's id.
      sortSockets :: [(T.PeerId, NS.Socket)] -> [(T.PeerId, NS.Socket)]
      sortSockets =
        let predicate lhs rhs | (fst lhs) < (fst rhs) = LT
                              | (fst lhs) > (fst rhs) = GT
                              | otherwise             = EQ
        in sortBy predicate

  in do
    unorderedSockets <- lift $ sockets

    -- Retrieve all peer ids
    peerIds        <- mapM handShake unorderedSockets

    -- Combine the sockets with the peer ids, sort them based on the peer id,
    -- and get a list of the sockets out of it.
    return (map snd (sortSockets (zip peerIds unorderedSockets)))

-- | In the second phase, the leader receives all the encrypted messages from all
--   the slaves.
--
--   This is a blocking operation.
phase2 :: [NS.Socket]                     -- ^ The Sockets we accepted
       -> ExceptT String IO [R.Encrypted] -- ^ All the encrypted messages we received from the
                                          --   slaves.
phase2 sockets = do
  ciphers <- liftIO $ mapM NS.receiveAndDecode sockets
  either throwE return (sequence ciphers)

-- | In the third phase, the leader sends all the ciphers to the first node
--   in the quorum.
--
--   Note that in our implementation, the first node is always the leader
--   itself.
phase3 :: [NS.Socket]    -- ^ All connections to all slaves
       -> [R.Encrypted]  -- ^ The ciphers we received from all slaves
       -> ExceptT String IO ()
phase3 sockets ciphers =
  let firstSocket :: NS.Socket
      firstSocket = head sockets

  in do
    liftIO $ NS.encodeAndSend firstSocket ciphers
    return ()
