module Api.Resource.Questionnaire.QuestionFlagDTO where

import GHC.Generics

data QuestionFlagTypeDTO
  = NeedsReview
  | MigrationResolved
    deriving (Generic, Show, Eq)

data QuestionFlagDTO = QuestionFlagDTO
  { _questionFlagDTOQuestionPath :: [String]
  , _questionFlagDTOFlagType :: QuestionFlagTypeDTO
  } deriving (Show, Eq)
