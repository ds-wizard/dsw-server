module Service.Report.Evaluator.Metric where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Maybe (catMaybes, isJust)
import Data.Time

import LensesConfig
import Model.Context.AppContext
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.Questionnaire.QuestionnaireReply
import Model.Report.Report
import Util.Math
import Util.Uuid

computeMetrics :: [Metric] -> KnowledgeModel -> [Reply] -> Chapter -> [MetricSummary]
computeMetrics metrics km replies ch = []
-- computeMetrics metrics fChapter = (computeMetricSummary fChapter) <$> metrics


-- computeMetricSummary :: FilledChapter -> Metric -> MetricSummary
-- computeMetricSummary fChapter m =
--   MetricSummary {_metricSummaryMetricUuid = m ^. uuid, _metricSummaryMeasure = msMeasure}
--   where
--     msMeasure :: Maybe Double
--     msMeasure =
--       computeWeightAverage .
--       filterAccordingCurrentMetric .
--       mapToMetricMeasures . catMaybes . map mapOptionQuestion . getAllFilledQuestionsForChapter $
--       fChapter
--     mapOptionQuestion :: FilledQuestion -> Maybe FilledOptionsQuestion
--     mapOptionQuestion (FilledOptionsQuestion' q) = Just q
--     mapOptionQuestion _ = Nothing
--     mapToMetricMeasures :: [FilledOptionsQuestion] -> [MetricMeasure]
--     mapToMetricMeasures =
--       concat . (map _filledAnswerMetricMeasures) . catMaybes . (map _filledOptionsQuestionAnswerOption)
--     filterAccordingCurrentMetric :: [MetricMeasure] -> [MetricMeasure]
--     filterAccordingCurrentMetric = filter (\mm -> mm ^. metricUuid == m ^. uuid)
--     computeWeightAverage :: [MetricMeasure] -> Maybe Double
--     computeWeightAverage [] = Nothing
--     computeWeightAverage mms = Just . weightAverage . fmap (\mm -> (mm ^. measure, mm ^. weight)) $ mms
--
