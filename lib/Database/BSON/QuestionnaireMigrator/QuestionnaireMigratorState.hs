module Database.BSON.QuestionnaireMigrator.QuestionnaireMigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Database.BSON.Questionnaire.Questionnaire ()
import Database.BSON.Event.Common
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

instance ToBSON QuestionnaireMigratorState where
  toBSON state =
    [ "questionnaire" BSON.=: (state ^. questionnaire)
    , "targetPackageId" BSON.=: (state ^. targetPackageId)
    ]

instance FromBSON QuestionnaireMigratorState where
  fromBSON doc = do
    questionnaire <- BSON.lookup "questionnaire" doc
    targetPackageId <- BSON.lookup "targetPackageId" doc
    return
      QuestionnaireMigratorState
      { _questionnaireMigratorStateQuestionnaire = questionnaire
      , _questionnaireMigratorStateTargetPackageId = targetPackageId
      }
