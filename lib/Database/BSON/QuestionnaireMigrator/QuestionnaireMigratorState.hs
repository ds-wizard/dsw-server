module Database.BSON.QuestionnaireMigrator.QuestionnaireMigratorState where

import qualified Data.UUID as U

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Common
import Database.BSON.Event.Common
import Database.BSON.Event.KnowledgeModel ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import LensesConfig
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

instance ToBSON QuestionnaireMigratorMigrationState where
  toBSON QuestionnaireMigrationRunning = ["stateType" BSON.=: "Running"]
  toBSON QuestionnaireMigratorConflict = ["stateType" BSON.=: "Conflict"]
  toBSON QuestionnaireMigratorError = ["stateType" BSON.=: "Error"]
  toBSON QuestionnaireMigratorCompleted = ["stateType" BSON.=: "Completed"]

instance FromBSON QuestionnaireMigratorMigrationState where
  fromBSON doc = do
    stateType <- BSON.lookup "stateType" doc
    case stateType of
      "Running"   -> return QuestionnaireMigrationRunning
      "Conflict"  -> return QuestionnaireMigratorConflict
      "Error"     -> return QuestionnaireMigratorError
      "Completed" -> return QuestionnaireMigratorCompleted

instance ToBSON QuestionnaireMigratorState where
  toBSON state =
    [ "questionnaireUuid" BSON.=: serializeUUID (_questionnaireMigratorQuestionnaireUuid state)
    , "migrationState" BSON.=: (_questionnaireMigratorMigrationState state)
    , "targetPackageId" BSON.=: (_questionnaireMigratorTargetPackageId state)
    , "targetPackageEvents" BSON.=: convertEventToBSON <$> (_questionnaireMigratorTargetPackageEvents state)
    , "currentKnowledgeModel" BSON.=: (_questionnaireMigratorCurrentKnowledgeModel state)
    ]

instance FromBSON QuestionnaireMigratorState where
  fromBSON doc = do
    uuid <- U.fromString $ BSON.lookup "questionnaireUuid" doc
    migrationState <- BSON.lookup "migrationState" doc
    targetPackageId <- BSON.lookup "targetPackageId" doc
    targetPackageEventsSerialized <- BSON.lookup "targetPackageEvents" doc
    let targetPackageEvents = fmap (fromJust . chooseEventDeserializator) targetPackageEventsSerialized
    currentKnowledgeModel <- BSON.lookup "currentKnowledgeModel" doc
    return
      QuestionnaireMigratorState
      { _questionnaireMigratorQuestionnaireUuid = uuid
      , _questionnaireMigratorMigrationState = migrationState
      , _questionnaireMigratorTargetPackageId = targetPackageId
      , _questionnaireMigratorTargetPackageEvents = targetPackageEvents
      , _questionnaireMigratorCurrentKnowledgeModel = currentKnowledgeModel
      }
