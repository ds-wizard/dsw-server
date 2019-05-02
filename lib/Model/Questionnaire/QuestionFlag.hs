module Model.Questionnaire.QuestionFlag where

import GHC.Generics

data QuestionFlagType
  = NeedsReview
  | MigrationResolved
  deriving (Generic, Show, Eq)

data QuestionFlags = QuestionFlags
  { _questionFlagsQuestionPath :: [String]
  , _questionFlagsFlagTypes :: [QuestionFlagType]
  } deriving (Show, Eq)
