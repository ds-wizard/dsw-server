module Api.Resource.Questionnaire.QuestionnaireStateDTO where

data QuestionnaireStateDTO
  = QSDefault
  | QSMigrating
  | QSOutdated
  deriving (Show, Eq)
