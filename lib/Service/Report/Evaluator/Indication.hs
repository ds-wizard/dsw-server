module Service.Report.Evaluator.Indication where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe (catMaybes, isJust)
import Data.Time
import qualified Data.List as L
import qualified Data.UUID as U

import LensesConfig
import Model.Context.AppContext
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelOldAccessors
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.KnowledgeModel.KnowledgeModelAccessors
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Util.Math
import Util.Uuid

computeIndications :: Int -> KnowledgeModel -> [Reply] -> Chapter -> [Indication]
computeIndications requiredLevel km replies ch = [computeAnsweredIndication requiredLevel km replies ch]

computeAnsweredIndication :: Int -> KnowledgeModel -> [Reply] -> Chapter -> Indication
computeAnsweredIndication requiredLevel km replies ch =
    AnsweredIndication' $
    AnsweredIndication
    { _answeredIndicationAnsweredQuestions = 0
    , _answeredIndicationUnansweredQuestions = 0
    }

computeChapter :: Int -> KnowledgeModel -> [Reply] -> Chapter -> Int
computeChapter requiredLevel km replies ch =
    let currentPath = U.toString . getEntityUuid $ ch
        qs = getQuestionsForChapterUuid km (ch ^. uuid)
    in sum . fmap (computeQuestion requiredLevel km replies currentPath) $ qs

computeQuestion :: Int -> KnowledgeModel -> [Reply] -> String -> Question -> Int
computeQuestion requiredLevel km replies path q =
  let currentPath = path ++ (U.toString . getEntityUuid $ q)
      -- qs = getQuestionsForChapterUuid km (ch ^. uuid)
  in
    case getReply replies currentPath of
      Just reply -> 1
      Nothing -> 0



getReply :: [Reply] -> String -> Maybe Reply
getReply replies p = L.find (\r -> r ^. path == p) replies





-- isQuestionAnswered :: FilledQuestion -> Bool
-- isQuestionAnswered (FilledOptionsQuestion' fq) = isJust $ fq ^. answerOption
-- isQuestionAnswered (FilledListQuestion' fq) =
--   case fq ^. items of
--     Nothing -> False
--     Just [] -> False
--     Just (x:xs) -> True
-- isQuestionAnswered (FilledValueQuestion' fq) = isJust $ fq ^. answerValue
-- isQuestionAnswered (FilledIntegrationQuestion' fq) = isJust $ fq ^. answerValue
--
-- isAIAnswered :: FilledAnswerItem -> Bool
-- isAIAnswered fai = isJust $ fai ^. value
--
-- computeAnsweredIndication :: Int -> FilledChapter -> Indication
-- computeAnsweredIndication requiredLevel fChapter =
--   AnsweredIndication' $
--   AnsweredIndication
--   { _answeredIndicationAnsweredQuestions =
--       sum $ getQuestionCount (isQuestionAnswered) (isAIAnswered) <$> fChapter ^. questions
--   , _answeredIndicationUnansweredQuestions =
--       sum $ getQuestionCount (not . isQuestionAnswered) (not . isAIAnswered) <$> fChapter ^. questions
--   }
--   where
--     getQuestionCount :: (FilledQuestion -> Bool) -> (FilledAnswerItem -> Bool) -> FilledQuestion -> Int
--     getQuestionCount conditionQ conditionAI fq = currentQuestion + (childrens fq)
--       where
--         currentQuestion =
--           if conditionQ fq && isRequiredNow
--             then 1
--             else 0
--         isRequiredNow =
--           case getRequiredLevel fq of
--             Just rl -> rl <= requiredLevel
--             Nothing -> True
--         childrens (FilledOptionsQuestion' fq) = walkOverAnswerOption $ fq ^. answerOption
--           where
--             walkOverAnswerOption mAo =
--               sum $ maybe [] (\ao -> (getQuestionCount conditionQ conditionAI) <$> ao ^. followUps) mAo
--         childrens (FilledListQuestion' fq) = walkOverAnswerItems $ fq ^. items
--           where
--             walkOverAnswerItems mAis = sum $ maybe [] (\ais -> walkOverAnswerItem <$> ais) mAis
--             walkOverAnswerItem ai =
--               let itemName =
--                     if conditionAI ai && isRequiredNow
--                       then 1
--                       else 0
--                   questionsCount = (sum $ (getQuestionCount conditionQ conditionAI) <$> ai ^. questions)
--               in itemName + questionsCount
--         childrens (FilledValueQuestion' fq) = 0
--         childrens (FilledIntegrationQuestion' fq) = 0
--
