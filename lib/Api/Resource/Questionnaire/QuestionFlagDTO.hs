module Api.Resource.Questionnaire.QuestionFlagDTO where

import GHC.Generics

data QuestionFlagTypeDTO
  = NeedsReview
  | MigrationResolved
    deriving (Generic, Show, Eq)

data QuestionFlagsDTO = QuestionFlagsDTO
  { _questionFlagsDTOQuestionPath :: [String]
  , _questionFlagsDTOFlagTypes :: [QuestionFlagTypeDTO]
  } deriving (Show, Eq)
