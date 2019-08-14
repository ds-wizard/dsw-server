module Service.KnowledgeModel.Compilator.Modifier.Chapter where

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

instance CreateEntity AddChapterEvent Chapter where
  createEntity e = Chapter {_chapterUuid = e ^. entityUuid, _chapterTitle = e ^. title, _chapterText = e ^. text, _chapterQuestionUuids = []}

instance EditEntity EditChapterEvent Chapter where
  editEntity e = applyQuestionUuids . applyText . applyTitle
    where
      applyTitle ch = applyValue (e ^. title) ch title
      applyText ch = applyValue (e ^. text) ch text
      applyQuestionUuids ch = applyValue (e ^. questionUuids) ch questionUuids
