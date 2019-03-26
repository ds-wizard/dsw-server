module Api.Resource.Questionnaire.QuestionFlagJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionFlagDTO

instance FromJSON QuestionFlagTypeDTO

instance ToJSON QuestionFlagTypeDTO

instance FromJSON QuestionFlagDTO where
  parseJSON (Object o) = do
    _questionFlagDTOQuestionPath <- o .: "questionPath"
    _questionFlagDTOFlagType <- o .: "flagType"
    return QuestionFlagDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionFlagDTO where
  toJSON QuestionFlagDTO {..} =
    object
      [ "questionPath" .= _questionFlagDTOQuestionPath
      , "flagType" .= _questionFlagDTOFlagType
      ]
