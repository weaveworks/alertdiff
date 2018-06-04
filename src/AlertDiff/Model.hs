module AlertDiff.Model
    ( LabelSet
    , Alert(..)
    , Metric(..)
    , diffAlerts
    , renderMetrics
    ) where

import Data.List                    (intercalate)
import Data.Set                     (Set)
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import AlertDiff.AlertManager.Model (Alert(..),LabelSet)

data Metric = Metric String LabelSet Double deriving (Show, Eq, Ord)

renderMetrics :: [Metric] -> String
renderMetrics = concatMap renderMetric
    where
        -- TODO key & value need escaping
        renderLabel :: String -> String -> [String]
        renderLabel key value = [ key ++ "=\"" ++ value ++ "\"" ]

        renderLabels :: LabelSet -> String
        renderLabels labelSet = intercalate "," $ Map.foldMapWithKey renderLabel labelSet

        renderMetric :: Metric -> String
        renderMetric (Metric name labels value) =
            name ++ "{" ++ renderLabels labels ++  "} " ++ show value ++  "\n"

-- Represent an alert (labels + annotations) as a set of labels. For example,
-- given an alert like this:
--
-- {
--     "labels" : { "status" : "500" }
--      "annotations" : { "detail" : "Internal server error"}
-- }
--
-- Return a label set like this:
--
-- { "label_status" : "500", "annotation_detail" : "Internal server error"}
--
-- This enables us to build a metric detailing a missing or spurious alert.
--
alertToLabelSet :: Alert -> LabelSet
alertToLabelSet alert = Map.union labelMap annotationMap
    where
        labelMap = Map.mapKeys ("label_" ++) (labels alert)
        annotationMap = Map.mapKeys ("annotation_" ++) (annotations alert)

-- Make a metric with labels derived from a set of alerts
alertsToMetrics :: String -> Set Alert -> Set Metric
alertsToMetrics metricName = Set.map alertToMetric
    where
        alertToMetric :: Alert -> Metric
        alertToMetric alert = Metric metricName (alertToLabelSet alert) 1.0 

-- Compare expected to actual alerts and return the differences
-- as metrics on which we can base Prometheus alerts.
diffAlerts
    :: Set String       -- ^ Labels to exclude from comparison
    -> [Alert]          -- ^ Expected alerts
    -> [Alert]          -- ^ Actual alerts
    -> [Metric]         -- ^ Difference expressed as metrics
diffAlerts excludedLabels expected actual = Set.toList $ Set.union missing spurious
    where
        es = Set.fromList $ excludeLabels <$> expected
        as = Set.fromList $ excludeLabels <$> actual
        missing = alertsToMetrics "alertdiff_missing_alert" $ Set.difference es as
        spurious = alertsToMetrics "alertdiff_spurious_alert" $ Set.difference as es

        excludeLabels :: Alert -> Alert
        excludeLabels alert =
            alert { labels = Map.withoutKeys (labels alert) excludedLabels }
