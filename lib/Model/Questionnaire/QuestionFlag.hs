module Model.Questionnaire.QuestionFlag where

import GHC.Generics

data QuestionFlagType
  = NeedsReview
  | MigrationResolved
  deriving (Generic, Show, Eq)

data QuestionFlag = QuestionFlag
  { _questionFlagQuestionPath :: [String]
  , _questionFlagFlagType :: QuestionFlagType
  }
  deriving (Show, Eq)
