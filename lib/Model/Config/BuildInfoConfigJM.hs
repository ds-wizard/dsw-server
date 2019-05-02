module Model.Config.BuildInfoConfigJM where

import Data.Aeson
import qualified Data.Text as T

import Model.Config.BuildInfoConfig
import Model.Config.EnvironmentJM ()
import Util.String (lowerFirst)

simpleParseJSON fieldPrefix = genericParseJSON opts
  where
    opts = defaultOptions {fieldLabelModifier = lowerFirst . drop (T.length fieldPrefix)}

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
