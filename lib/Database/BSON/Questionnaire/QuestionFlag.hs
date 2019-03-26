module Database.BSON.Questionnaire.QuestionFlag where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Model.Questionnaire.QuestionFlag

instance ToBSON QuestionFlag where
  toBSON model =
    [ "questionPath" BSON.=: (model ^. questionPath)
    , "flagType" BSON.=: model ^. flagType
    ]

instance FromBSON QuestionFlag where
  fromBSON doc = do
    questionPath <- BSON.lookup "questionPath" doc
    flagType <- BSON.lookup "flagType" doc
    return
     QuestionFlag
      { _questionFlagQuestionPath = questionPath
      , _questionFlagFlagType = flagType
      }

instance ToBSON QuestionFlagType

instance FromBSON QuestionFlagType
