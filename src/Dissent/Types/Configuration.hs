-- | Describes configuration information about our ourself, information that is
--   required to launch our service.

module Dissent.Types.Configuration where

import Network

import qualified Dissent.Types.Self   as TS

-- | Our configuration object which hold all data we need to 'bootstrap'
--   our entire quorum and protocol.
data Configuration = Configuration {

  -- | Our hostname, reachable from the outside world
  host ::          HostName,

  -- | The port we listen at
  port ::          PortNumber,

  -- | Initial state information about ourselves
  self ::          TS.Self

  } deriving (Eq, Show)
