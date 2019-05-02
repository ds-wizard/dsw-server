module Api.Resource.Questionnaire.QuestionnaireStateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireStateDTO

instance FromJSON QuestionnaireStateDTO where
  parseJSON (Object o) = do
    state <- o .: "questionnaireState"
    case state of
      "Default" -> return QSDefault
      "Migrating" -> return QSMigrating
      "Outdated" -> return QSOutdated
      _ -> fail "Unsupported questionnaire state"
  parseJSON _ = mzero

instance ToJSON QuestionnaireStateDTO where
  toJSON QSDefault = "Default"
  toJSON QSMigrating = "Migrating"
  toJSON QSOutdated = "Outdated"
