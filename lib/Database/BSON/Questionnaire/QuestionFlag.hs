module Database.BSON.Questionnaire.QuestionFlag where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Model.Questionnaire.QuestionFlag

instance ToBSON QuestionFlags where
  toBSON model = ["questionPath" BSON.=: (model ^. questionPath), "flagTypes" BSON.=: model ^. flagTypes]

instance FromBSON QuestionFlags where
  fromBSON doc = do
    questionPath <- BSON.lookup "questionPath" doc
    flagTypes <- BSON.lookup "flagTypes" doc
    return QuestionFlags {_questionFlagsQuestionPath = questionPath, _questionFlagsFlagTypes = flagTypes}

instance ToBSON QuestionFlagType

instance FromBSON QuestionFlagType
