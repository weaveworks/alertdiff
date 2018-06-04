{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module AlertDiff.Server
    ( AlertDiffServerAPI
    , AppM
    , alertDiffServer
    , app
    ) where

import Control.Concurrent.Async                (concurrently)
import Control.Concurrent.STM                  (readTVar, writeTVar)
import Control.Monad.IO.Class                  (liftIO)
import Control.Monad.STM                       (atomically)
import Control.Monad.Trans.Reader              (ReaderT, ask)
import Control.Monad.Trans.Reader              (runReaderT)
import Data.Set                                (Set)
import Network.HTTP.Client                     (Manager)
import qualified Data.ByteString.Lazy.Char8 as BS
import Servant.Client                          (ServantError,runClientM,showBaseUrl,mkClientEnv,parseBaseUrl)
import Servant                                 ((:<|>)(..),(:>),Get,JSON,PlainText,NoContent(..),MimeRender(..))
import Servant                                 (Proxy(..),ServerT,Application,Handler,hoistServer,serve)
import Servant                                 (ServantErr,err400,err502,errBody,throwError)

import AlertDiff.AlertManager.Client           (AlertResponse(..),getAlertResponse)
import AlertDiff.AlertManager.Server           (AlertManagerServerAPI)
import AlertDiff                               (AlertSource(..),State(..))
import AlertDiff.Model                         (Alert,Metric,diffAlerts,renderMetrics)

-- | The Alertdiff server API definition
type AlertDiffServerAPI = "expected" :> AlertManagerServerAPI
               :<|> "actual"   :> AlertManagerServerAPI
               :<|> "metrics"  :> Get '[PlainText] [Metric]

-- Use a custom monad that stores our configuration and state
type AppM = ReaderT State Handler

api :: Proxy AlertDiffServerAPI
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

-- Server implementation
alertDiffServer :: ServerT AlertDiffServerAPI AppM
alertDiffServer = postAlerts expectedSource :<|> postAlerts actualSource :<|> getMetrics
  where
    postAlerts :: (State -> AlertSource) -> [Alert] -> AppM NoContent
    postAlerts source alerts = do
        state <- ask 
        case source state of
            PushSource t -> liftIO $ do
                atomically $ writeTVar t alerts
                pure NoContent
            PullSource url _ -> throwError err400 { errBody = BS.pack $ "Push disabled (pulling from " ++ showBaseUrl url ++ ")"}

    getMetrics :: AppM [Metric]
    getMetrics = do
        State{manager, expectedSource, actualSource, isImportant, excludedLabels} <- ask
        (expected, actual) <- liftIO $ concurrently (getAlerts manager expectedSource) (getAlerts manager actualSource)
        let expected' = filter isImportant <$> expected
        let actual' = filter isImportant <$> actual
        returnDiff excludedLabels expected' actual'

    getAlerts :: IO Manager -> AlertSource -> IO (Either ServantErr [Alert])
    getAlerts _ (PushSource t) = Right <$> (atomically $ readTVar t)
    getAlerts manager (PullSource url authHeader) = do
        manager' <- manager
        liftError <$> runClientM (getAlertResponse authHeader) (mkClientEnv manager' url)

    liftError :: Either ServantError AlertResponse -> Either ServantErr [Alert]
    liftError (Right ar@AlertResponse{status = "success"}) = Right $ alerts ar
    liftError (Right ar) = Left err502 { errBody = BS.pack $ status ar }
    liftError (Left err) = Left err502 { errBody = BS.pack $ show err }

    returnDiff :: Set String -> Either ServantErr [Alert] -> Either ServantErr [Alert] -> AppM [Metric]
    returnDiff excludedLabels (Right expected) (Right actual) = pure $ diffAlerts excludedLabels expected actual
    returnDiff _ (Left expected) (Left actual) = throwError err502 { errBody = BS.pack $ show expected ++ show actual }
    returnDiff _ (Left err) _ = throwError err502 { errBody = BS.pack $ show err }
    returnDiff _ _ (Left err) = throwError err502 { errBody = BS.pack $ show err }

-- | Teach Servant how to convert a list of metrics into Prometheus exposition
-- format.
instance MimeRender PlainText [Metric] where
    mimeRender _ = BS.pack . renderMetrics

-- Runnable application
app :: State -> Application
app s = serve api $ hoistServer api (nt s) alertDiffServer
