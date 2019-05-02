module Model.Config.AppConfigJM where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Model.Config.AppConfig
import Model.Config.EnvironmentJM ()
import Util.String (lowerFirst)

simpleParseJSON fieldPrefix = genericParseJSON opts
  where
    opts = defaultOptions {fieldLabelModifier = lowerFirst . drop (T.length fieldPrefix)}

instance FromJSON AppConfig where
  parseJSON = simpleParseJSON "_appConfig"

instance FromJSON AppConfigGeneral where
  parseJSON = simpleParseJSON "_appConfigGeneral"

instance FromJSON AppConfigClient where
  parseJSON = simpleParseJSON "_appConfigClient"

instance FromJSON AppConfigDatabase where
  parseJSON = simpleParseJSON "_appConfigDatabase"

instance FromJSON AppConfigMessaging where
  parseJSON = simpleParseJSON "_appConfigMessaging"

instance FromJSON AppConfigJwt where
  parseJSON = simpleParseJSON "_appConfigJwt"

instance FromJSON AppConfigRoles where
  parseJSON = simpleParseJSON "_appConfigRoles"

instance FromJSON AppConfigMail where
  parseJSON = simpleParseJSON "_appConfigMail"

instance FromJSON AppConfigAnalytics where
  parseJSON = simpleParseJSON "_appConfigAnalytics"

instance FromJSON AppConfigFeedback where
  parseJSON = simpleParseJSON "_appConfigFeedback"

jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where
    lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x
