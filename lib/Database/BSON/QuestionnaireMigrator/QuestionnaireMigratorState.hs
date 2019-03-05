module Database.BSON.QuestionnaireMigrator.QuestionnaireMigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import LensesConfig
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.BSON.Questionnaire.Questionnaire ()
import Database.BSON.Event.Common
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

instance ToBSON QuestionnaireMigratorState where
  toBSON state =
    [ "questionnaire" BSON.=: (state ^. questionnaire)
    , "diffKnowledgeModel" BSON.=: (state ^. diffKnowledgeModel)
    , "targetPackageId" BSON.=: (state ^. targetPackageId)
    , "diffEvents" BSON.=: convertEventToBSON <$> (state ^. diffEvents)
    ]

instance FromBSON QuestionnaireMigratorState where
  fromBSON doc = do
    questionnaire <- BSON.lookup "questionnaire" doc
    diffKnowledgeModel <- BSON.lookup "diffKnowledgeModel" doc
    targetPackageId <- BSON.lookup "targetPackageId" doc
    serializedEvents <- BSON.lookup "diffEvents" doc
    let events = (fromJust . chooseEventDeserializator) <$> serializedEvents
    return
      QuestionnaireMigratorState
      { _questionnaireMigratorStateQuestionnaire = questionnaire
      , _questionnaireMigratorStateDiffKnowledgeModel = diffKnowledgeModel
      , _questionnaireMigratorStateTargetPackageId = targetPackageId
      , _questionnaireMigratorStateDiffEvents = events
      }
