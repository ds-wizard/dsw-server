module Model.QuestionnaireMigrator.QuestionnaireMigratorState where

import Model.Questionnaire.Questionnaire
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelDiff.DiffEvent

data QuestionnaireMigratorState = QuestionnaireMigratorState
  { _questionnaireMigratorStateQuestionnaire :: Questionnaire
  , _questionnaireMigratorStateDiffKnowledgeModel :: KnowledgeModel
  , _questionnaireMigratorStateDiffEvents :: [DiffEvent]
  , _questionnaireMigratorStateTargetPackageId :: String
  } deriving (Show, Eq)
