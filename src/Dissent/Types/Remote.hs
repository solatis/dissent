-- | Describes types of remote hosts that we can communicate with.

module Dissent.Types.Remote where

import           Network
import           Dissent.Crypto.Rsa as R (PublicKey)

-- | Hostname, Port and RSA public key
data Remote = Remote {
  -- | Hostname of peer. This can be a "real" hostname, or a IPv4/IPv6
  --   address.
  hostName            :: HostName,

  -- | Port the peer listens at
  port                :: PortNumber,

  -- | Public key of the peer used for signing
  publicSigningKey    :: R.PublicKey,

  -- | Public key of the peer used for encryption
  publicEncryptionKey :: R.PublicKey

  } deriving (Eq, Show, Ord)
