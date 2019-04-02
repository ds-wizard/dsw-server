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

fromQuestionFlagDTO :: QD.QuestionFlagsDTO -> QF.QuestionFlags
fromQuestionFlagDTO dto =
  QF.QuestionFlags
    { _questionFlagsQuestionPath = dto ^. questionPath
    , _questionFlagsFlagTypes = map fromQuestionFlagTypeDTO $ dto ^. flagTypes
    }

toQuestionFlagDTO :: QF.QuestionFlags -> QD.QuestionFlagsDTO
toQuestionFlagDTO model =
  QD.QuestionFlagsDTO
    { _questionFlagsDTOQuestionPath = model ^. questionPath
    , _questionFlagsDTOFlagTypes = map toQuestionFlagTypeDTO $ model ^. flagTypes
    }
