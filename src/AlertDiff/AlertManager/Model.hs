{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AlertDiff.AlertManager.Model
    ( LabelSet,
      AlertResponse(..),
      Alert(..),
    ) where

import qualified Data.Map.Strict as Map
import Data.Aeson                   (FromJSON(..),Value(Object),(.:))
import Data.Map.Strict              (Map)
import GHC.Generics                 (Generic)

type LabelSet = Map String String

data Alert =
    Alert {labels      :: LabelSet
          ,annotations :: LabelSet
          } deriving (Show, Generic, Eq, Ord)

instance FromJSON Alert

data AlertResponse =
    AlertResponse {status :: String
                  ,alerts :: [Alert]
                  } deriving (Show)

-- Can't derive this generically because `data` is reserved
instance FromJSON AlertResponse
    where
        parseJSON (Object v) = AlertResponse <$> v .: "status" <*> v .: "data"
