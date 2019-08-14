module Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel where

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

instance CreateEntity AddKnowledgeModelEvent KnowledgeModel where
  createEntity e =
    KnowledgeModel
    { _knowledgeModelUuid = e ^. entityUuid
    , _knowledgeModelName = e ^. name
    , _knowledgeModelChapterUuids = []
    , _knowledgeModelTagUuids = []
    , _knowledgeModelIntegrationUuids = []
    , _knowledgeModelEntities =
        KnowledgeModelEntities
        { _knowledgeModelEntitiesChapters = M.empty
        , _knowledgeModelEntitiesQuestions = M.empty
        , _knowledgeModelEntitiesAnswers = M.empty
        , _knowledgeModelEntitiesExperts = M.empty
        , _knowledgeModelEntitiesReferences = M.empty
        , _knowledgeModelEntitiesIntegrations = M.empty
        , _knowledgeModelEntitiesTags = M.empty
        }
    }

instance EditEntity EditKnowledgeModelEvent KnowledgeModel where
  editEntity e = applyIntegrationUuids . applyTagUuids . applyChapterUuids . applyName
    where
      applyName km = applyValue (e ^. name) km name
      applyChapterUuids km = applyValue (e ^. chapterUuids) km chapterUuids
      applyTagUuids km = applyValue (e ^. tagUuids) km tagUuids
      applyIntegrationUuids km = applyValue (e ^. integrationUuids) km integrationUuids
