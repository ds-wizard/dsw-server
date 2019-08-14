module Service.KnowledgeModel.Compilator.Modifier.Expert where

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

instance CreateEntity AddExpertEvent Expert where
  createEntity e = Expert {_expertUuid = e ^. entityUuid, _expertName = e ^. name, _expertEmail = e ^. email}

instance EditEntity EditExpertEvent Expert where
  editEntity e = applyEmail . applyName
    where
      applyName exp = applyValue (e ^. name) exp name
      applyEmail exp = applyValue (e ^. email) exp email
