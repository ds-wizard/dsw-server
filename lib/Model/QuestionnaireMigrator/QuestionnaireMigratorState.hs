module Model.QuestionnaireMigrator.QuestionnaireMigratorState where

import qualified Data.UUID as U

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data QuestionnaireMigratorMigrationState
    = QuestionnaireMigrationRunning
    | QuestionnaireMigratorConflict
    | QuestionnaireMigratorError
    | QuestionnaireMigratorCompleted
    deriving (Show, Eq)

data QuestionnaireMigratorState = QuestionnaireMigratorState
    { _questionnaireMigratorQuestionnaireUuid :: U.UUID
    , _questionnaireMigratorMigrationState :: QuestionnaireMigratorMigrationState
    , _questionnaireMigratorTargetPackageId :: String
    , _questionnaireMigratorTargetPackageEvents :: [Event]
    , _questionnaireMigratorCurrentKnowledgeModel :: KnowledgeModel
    } deriving (Show, Eq)
