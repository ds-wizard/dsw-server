module Service.Questionnaire.QuestionFlagMapper where

import Control.Lens ((^.))

import LensesConfig
import qualified Api.Resource.Questionnaire.QuestionFlagDTO as QD
import qualified Model.Questionnaire.QuestionFlag as QF

fromQuestionFlagTypeDTO :: QD.QuestionFlagTypeDTO -> QF.QuestionFlagType
fromQuestionFlagTypeDTO QD.NeedsReview = QF.NeedsReview
fromQuestionFlagTypeDTO QD.MigrationResolved = QF.MigrationResolved

toQuestionFlagTypeDTO :: QF.QuestionFlagType -> QD.QuestionFlagTypeDTO
toQuestionFlagTypeDTO QF.NeedsReview = QD.NeedsReview
toQuestionFlagTypeDTO QF.MigrationResolved = QD.MigrationResolved

fromQuestionFlagDTO :: QD.QuestionFlagDTO -> QF.QuestionFlag
fromQuestionFlagDTO dto =
  QF.QuestionFlag
    { _questionFlagQuestionPath = dto ^. questionPath
    , _questionFlagFlagType = fromQuestionFlagTypeDTO $ dto ^. flagType
    }

toQuestionFlagDTO :: QF.QuestionFlag -> QD.QuestionFlagDTO
toQuestionFlagDTO model =
  QD.QuestionFlagDTO
    { _questionFlagDTOQuestionPath = model ^. questionPath
    , _questionFlagDTOFlagType = toQuestionFlagTypeDTO $ model ^. flagType
    }
