module Database.BSON.QuestionnaireMigrator.QuestionnaireMigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.BSON.Questionnaire.Questionnaire ()
import LensesConfig
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

instance ToBSON QuestionnaireMigratorState where
  toBSON state =
    [ "questionnaire" BSON.=: (state ^. questionnaire)
    , "diffKnowledgeModel" BSON.=: (state ^. diffKnowledgeModel)
    , "targetPackageId" BSON.=: serializeUUID (state ^. targetPackageId)
    ]

instance FromBSON QuestionnaireMigratorState where
  fromBSON doc = do
    questionnaire <- BSON.lookup "questionnaire" doc
    diffKnowledgeModel <- BSON.lookup "diffKnowledgeModel" doc
    targetPackageId <- deserializeMaybeUUID $ BSON.lookup "targetPackageId" doc
    return
      QuestionnaireMigratorState
      { _questionnaireMigratorStateQuestionnaire = questionnaire
      , _questionnaireMigratorStateDiffKnowledgeModel = diffKnowledgeModel
      , _questionnaireMigratorStateTargetPackageId = targetPackageId
      }
