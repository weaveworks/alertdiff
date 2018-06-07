module Main where

import Control.Concurrent.STM       (newTVar)
import Control.Monad.STM            (atomically)

import Network.HTTP.Client          (Manager,newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Network.Wai.Handler.Warp     (run)
import Options.Applicative          (execParser)
import Servant.Client               (parseBaseUrl)

import AlertDiff.AlertManager.Model (Alert(..))
import AlertDiff.Config             (Options(..),optionParser)
import AlertDiff.Model              (mkAlertDiffer)
import AlertDiff.Server             (app)
import AlertDiff                    (State(State),AlertSource(..))

main :: IO ()
main = do
    options         <- execParser optionParser
    let manager     = newManager tlsManagerSettings
    expectedSource  <- mkSource (expectedURL options) (expectedTokenFile options)
    actualSource    <- mkSource (actualURL options) (actualTokenFile options)
    let alertDiffer = mkAlertDiffer (excludedAlarms options) (excludedLabels options)

    run (port options) $ app $ State manager expectedSource actualSource alertDiffer

    where
        -- Return a push or pull alert source depending on supplied arguments
        mkSource :: Maybe String -> Maybe String -> IO AlertSource
        mkSource Nothing (Just _) = fail "Token specified without URL"
        mkSource Nothing _ = PushSource <$> (atomically $ newTVar [])
        mkSource (Just url) token = do
            url'   <- parseBaseUrl url
            token' <- traverse readFile token
            pure $ PullSource url' $ ("Scope-Probe token="++) <$> token'
