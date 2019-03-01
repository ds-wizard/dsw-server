module Model.QuestionnaireMigrator.QuestionnaireMigratorState where

import qualified Data.UUID as U

import Model.Questionnaire.Questionnaire
import Model.KnowledgeModel.KnowledgeModel

data QuestionnaireMigratorState = QuestionnaireMigratorState
    { _questionnaireMigratorStateQuestionnaire :: Questionnaire
    , _questionnaireMigratorStateDiffKnowledgeModel :: KnowledgeModel
    , _questionnaireMigratorStateTargetPackageId :: U.UUID
    } deriving (Show, Eq)
