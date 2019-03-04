module Database.BSON.QuestionnaireMigrator.QuestionnaireMigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.BSON.Questionnaire.Questionnaire ()
import Database.BSON.KnowledgeModelDiff.DiffEvent ()
import LensesConfig
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

instance ToBSON QuestionnaireMigratorState where
  toBSON state =
    [ "questionnaire" BSON.=: (state ^. questionnaire)
    , "diffKnowledgeModel" BSON.=: (state ^. diffKnowledgeModel)
    , "targetPackageId" BSON.=: (state ^. targetPackageId)
    , "diffEvents" BSON.=: (state ^. diffEvents)
    ]

instance FromBSON QuestionnaireMigratorState where
  fromBSON doc = do
    questionnaire <- BSON.lookup "questionnaire" doc
    diffKnowledgeModel <- BSON.lookup "diffKnowledgeModel" doc
    targetPackageId <- BSON.lookup "targetPackageId" doc
    events <- BSON.lookup "diffEvents" doc
    return
      QuestionnaireMigratorState
      { _questionnaireMigratorStateQuestionnaire = questionnaire
      , _questionnaireMigratorStateDiffKnowledgeModel = diffKnowledgeModel
      , _questionnaireMigratorStateTargetPackageId = targetPackageId
      , _questionnaireMigratorStateDiffEvents = events
      }
