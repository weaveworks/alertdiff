module AlertDiff
    ( AlertSource(..),
      State(..)
    ) where

import Control.Concurrent.STM        (TVar)
import Network.HTTP.Client           (Manager)
import Servant.Client                (BaseUrl)

import AlertDiff.AlertManager.Client (AuthToken)
import AlertDiff.AlertManager.Model  (Alert)

-- | The purpose of AlertDiff is to compare two sets of alerts; one expected
-- and one actual. These sets of alerts can either be pushed to us by
-- Prometheus, or we can poll for them from AlertManager. The mode of operation
-- can be specified on a per-source basis.
--
-- In the case of alerts being pushed to us, we use a PushSource to keep a
-- reference to some mutable state (the latest list of alerts) so that we have
-- something to compare against when we are asked to perform a comparison.
--
-- In the case of us having to pull alerts, we use a PullSource to store the
-- URL of the AlertManager from which to pull, and an optional token with which
-- to authenticate.
data AlertSource = PushSource (TVar [Alert]) | PullSource BaseUrl (Maybe AuthToken)

-- | Environment for our application
data State =
    State { manager        :: IO Manager    -- ^ HTTP connection manager for pulling alerts
          , expectedSource :: AlertSource   -- ^ Pull or push configuration for expected alerts
          , actualSource   :: AlertSource   -- ^ Push or push configuration for actual alerts
          , isImportant    :: Alert -> Bool -- ^ Predicate to filter out alerts from comparison
          }
