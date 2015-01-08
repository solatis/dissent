-- | Describes information about our ourself, information that *can* change, but
--   that is required in order for our protocol to function.
--
--   This can mean that state information is stored inside these objects.

module Dissent.Types.Self where

import qualified Dissent.Crypto.Rsa   as CR

-- | The secret keys we (currently) hold.
data Secret = Secret {

  -- | Our public/private signing KeyPair
  signingKey ::    CR.KeyPair,

  -- | Our public/private encryption KeyPair
  encryptionKey :: CR.KeyPair

  } deriving (Eq, Show)

data Self = Self {

  -- | Our secret keys
  secret :: Secret

  } deriving (Eq, Show)
