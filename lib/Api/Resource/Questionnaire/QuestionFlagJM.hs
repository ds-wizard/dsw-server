module Api.Resource.Questionnaire.QuestionFlagJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionFlagDTO

instance FromJSON QuestionFlagTypeDTO

instance ToJSON QuestionFlagTypeDTO

instance FromJSON QuestionFlagsDTO where
  parseJSON (Object o) = do
    _questionFlagsDTOQuestionPath <- o .: "questionPath"
    _questionFlagsDTOFlagTypes <- o .: "flagTypes"
    return QuestionFlagsDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionFlagsDTO where
  toJSON QuestionFlagsDTO {..} =
    object
      [ "questionPath" .= _questionFlagsDTOQuestionPath
      , "flagTypes" .= _questionFlagsDTOFlagTypes
      ]
