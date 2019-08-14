module Model.KnowledgeModel.KnowledgeModelOldAccessors where

import Control.Lens
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.KnowledgeModel.KnowledgeModelAccessors

-- -------------------
-- QUESTIONS----------
-- -------------------
getAllQuestionUuidsForChapterUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAllQuestionUuidsForChapterUuid km chUuid =
  case M.lookup chUuid (km ^. chaptersM) of
    Just chapter -> chapter ^. questionUuids
    Nothing -> []

getQuestionsForChapterUuid :: KnowledgeModel -> U.UUID -> [Question]
getQuestionsForChapterUuid km chUuid =
  case M.lookup chUuid (km ^. chaptersM) of
    Just ch -> foldl go [] (ch ^. questionUuids)
    Nothing -> []
  where
    go acc qUuid =
      case M.lookup (qUuid) (km ^. questionsM) of
        Just q -> acc ++ [q]
        Nothing -> acc

getAllQuestionUuidsForAnswerUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAllQuestionUuidsForAnswerUuid km ansUuid =
  case M.lookup ansUuid (km ^. answersM) of
    Just ans -> ans ^. followUpUuids
    Nothing -> []

getQuestionsForAnswerUuid :: KnowledgeModel -> U.UUID -> [Question]
getQuestionsForAnswerUuid km ansUuid =
  case M.lookup ansUuid (km ^. answersM) of
    Just ans -> foldl go [] (ans ^. followUpUuids)
    Nothing -> []
  where
    go acc qUuid =
      case M.lookup (qUuid) (km ^. questionsM) of
        Just q -> acc ++ [q]
        Nothing -> acc

getAllItemTemplateQuestionUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAllItemTemplateQuestionUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just (ListQuestion' q) -> q ^. itemTemplateQuestionUuids
    Nothing -> []

getItemTemplateQuestionsForQuestionUuid :: KnowledgeModel -> U.UUID -> [Question]
getItemTemplateQuestionsForQuestionUuid km qUuid =
  case M.lookup qUuid (km ^. questionsM) of
    Just q -> foldl go [] (q ^. itemTemplateQuestionUuids')
    Nothing -> []
  where
    go acc itqUuid =
      case M.lookup (itqUuid) (km ^. questionsM) of
        Just itq -> acc ++ [itq]
        Nothing -> acc


-- -------------------
-- EXPERT ------------
-- -------------------
getAllExpertUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAllExpertUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just question -> question ^. expertUuids'
    Nothing -> []

-- -------------------
-- REFERENCE ---------
-- -------------------
getAllReferenceUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAllReferenceUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just question -> question ^. referenceUuids'
    Nothing -> []

-- -------------------
-- ANSWER ------------
-- -------------------
getAllAnswerUuidsForQuestionUuid :: KnowledgeModel -> U.UUID -> [U.UUID]
getAllAnswerUuidsForQuestionUuid km questionUuid =
  case M.lookup questionUuid (km ^. questionsM) of
    Just (OptionsQuestion' q) -> q ^. answerUuids
    _ -> []
