module Model.QuestionnaireMigrator.QuestionnaireMigratorState where

import Model.Questionnaire.Questionnaire

data QuestionnaireMigratorState = QuestionnaireMigratorState
  { _questionnaireMigratorStateQuestionnaire :: Questionnaire
  , _questionnaireMigratorStateTargetPackageId :: String
  } deriving (Show, Eq)
