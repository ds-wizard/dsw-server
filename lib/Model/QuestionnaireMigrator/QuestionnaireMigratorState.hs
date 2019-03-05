module Model.QuestionnaireMigrator.QuestionnaireMigratorState where

import Model.Questionnaire.Questionnaire
import Model.KnowledgeModel.KnowledgeModel
import Model.Event.Event

data QuestionnaireMigratorState = QuestionnaireMigratorState
  { _questionnaireMigratorStateQuestionnaire :: Questionnaire
  , _questionnaireMigratorStateDiffKnowledgeModel :: KnowledgeModel
  , _questionnaireMigratorStateDiffEvents :: [Event]
  , _questionnaireMigratorStateTargetPackageId :: String
  } deriving (Show, Eq)
