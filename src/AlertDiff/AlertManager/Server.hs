{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AlertDiff.AlertManager.Server
    ( AlertManagerServerAPI
    ) where

import Servant ((:>),JSON,NoContent,Post,ReqBody)

import AlertDiff.Model (Alert)

-- | AlertManager API as documented here https://prometheus.io/docs/alerting/clients/
type AlertManagerServerAPI = "api" :> "v1" :> "alerts" :> ReqBody '[JSON] [Alert] :> Post '[JSON] NoContent
