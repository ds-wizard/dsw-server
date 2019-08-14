module Service.KnowledgeModel.Compilator.Modifier.Answer where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import qualified Data.List as L

import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.KnowledgeModel.KnowledgeModelOldLenses
import Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddAnswerEvent Answer where
  createEntity e =
    Answer
    { _answerUuid = e ^. entityUuid
    , _answerLabel = e ^. label
    , _answerAdvice = e ^. advice
    , _answerFollowUpUuids = []
    , _answerMetricMeasures = e ^. metricMeasures
    }

instance EditEntity EditAnswerEvent Answer where
  editEntity e = applyMetricMeasures . applyFollowUpUuids . applyAdvice . applyLabel
    where
      applyLabel ans = applyValue (e ^. label) ans label
      applyAdvice ans = applyValue (e ^. advice) ans advice
      applyFollowUpUuids ans = applyValue (e ^. followUpUuids) ans followUpUuids
      applyMetricMeasures ans = applyValue (e ^. metricMeasures) ans metricMeasures
