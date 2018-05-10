{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AlertDiff.AlertManager.Client
    ( AlertResponse(..)
    , AuthToken
    , getAlertResponse
    ) where

import Servant.Client               (ClientM,client)
import Servant                      ((:>),Proxy(..),Header,Get,JSON)

import AlertDiff.AlertManager.Model (AlertResponse(..))

type AuthToken = String

type AlertManagerClientAPI = "api" :> "v1" :> "alerts" :> Header "Authorization" AuthToken :> Get '[JSON] AlertResponse

api :: Proxy AlertManagerClientAPI
api = Proxy

-- | Get some alerts with optional authentication
getAlertResponse :: Maybe AuthToken -> ClientM AlertResponse
getAlertResponse = client api
