module Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import Data.Time
import qualified Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.Questionnaire.QuestionnaireStateDTO
import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Api.Resource.Questionnaire.QuestionFlagDTO
import Api.Resource.Questionnaire.QuestionnaireReplyJS ()

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid :: U.UUID
  , _questionnaireDetailDTOName :: String
  , _questionnaireDetailDTOLevel :: Int
  , _questionnaireDetailDTOPrivate :: Bool
  , _questionnaireDetailDTOState :: QuestionnaireStateDTO
  , _questionnaireDetailDTOPackage :: PackageDTO
  , _questionnaireDetailDTOSelectedTagUuids :: [U.UUID]
  , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireDetailDTOReplies :: [ReplyDTO]
  , _questionnaireDetailDTOOwnerUuid :: Maybe U.UUID
  , _questionnaireDetailDTOCreatedAt :: UTCTime
  , _questionnaireDetailDTOUpdatedAt :: UTCTime
  , _questionnaireDetailDTOQuestionFlags :: [QuestionFlagsDTO]
  } deriving (Show)

instance Eq QuestionnaireDetailDTO where
  a == b =
    _questionnaireDetailDTOUuid a == _questionnaireDetailDTOUuid b &&
    _questionnaireDetailDTOName a == _questionnaireDetailDTOName b &&
    _questionnaireDetailDTOLevel a == _questionnaireDetailDTOLevel b &&
    _questionnaireDetailDTOPrivate a == _questionnaireDetailDTOPrivate b &&
    _questionnaireDetailDTOPackage a == _questionnaireDetailDTOPackage b &&
    _questionnaireDetailDTOSelectedTagUuids a == _questionnaireDetailDTOSelectedTagUuids b &&
    _questionnaireDetailDTOKnowledgeModel a == _questionnaireDetailDTOKnowledgeModel b &&
    _questionnaireDetailDTOReplies a == _questionnaireDetailDTOReplies b &&
    _questionnaireDetailDTOOwnerUuid a == _questionnaireDetailDTOOwnerUuid b &&
    _questionnaireDetailDTOQuestionFlags a == _questionnaireDetailDTOQuestionFlags b
