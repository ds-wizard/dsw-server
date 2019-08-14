module Service.KnowledgeModel.Compilator.Modifier.Modifier where

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

applyValue (ChangedValue val) ch setter = ch & setter .~ val
applyValue NothingChanged ch setter = ch

class CreateEntity event entity where
  createEntity :: event -> entity

class EditEntity event entity where
  editEntity :: event -> entity -> entity
