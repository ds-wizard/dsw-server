module Service.KnowledgeModel.Compilator.EventApplicator.Reference where

import Control.Lens
import qualified Data.List as L
import Data.Map (empty, fromList, lookup)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.UUID as U
import Prelude hiding (lookup)

import LensesConfig
import Model.Error.Error
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.EventAccessors
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.Integration.IntegrationEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.Event.Tag.TagEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Model.KnowledgeModel.KnowledgeModelAccessors
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Question
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Service.KnowledgeModel.Compilator.Modifier.Modifier
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.Modifier.Delete

instance ApplyEvent AddReferenceEvent where
  apply =
    applyCreateEventWithParent (entities . references) (entities . questions) referenceUuids' "Reference" "Question"

instance ApplyEvent EditReferenceEvent where
  apply = applyEditEvent (entities . references) "Reference"

instance ApplyEvent DeleteReferenceEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event questionsM referenceUuids' $ deleteReference km (getEventNodeUuid event)
