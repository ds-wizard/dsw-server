module Service.KnowledgeModel.Compilator.Modifier.Integration where

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

instance CreateEntity AddIntegrationEvent Integration where
  createEntity e =
    Integration
    { _integrationUuid = e ^. entityUuid
    , _integrationIId = e ^. iId
    , _integrationName = e ^. name
    , _integrationProps = e ^. props
    , _integrationLogo = e ^. logo
    , _integrationRequestMethod = e ^. requestMethod
    , _integrationRequestUrl = e ^. requestUrl
    , _integrationRequestHeaders = e ^. requestHeaders
    , _integrationRequestBody = e ^. requestBody
    , _integrationResponseListField = e ^. responseListField
    , _integrationResponseIdField = e ^. responseIdField
    , _integrationResponseNameField = e ^. responseNameField
    , _integrationItemUrl = e ^. itemUrl
    }

instance EditEntity EditIntegrationEvent Integration where
  editEntity e =
    applyIId .
    applyName .
    applyProps .
    applyLogo .
    applyRequestMethod .
    applyRequestUrl .
    applyRequestHeaders .
    applyRequestBody . applyResponseListField . applyResponseIdField . applyResponseNameField . applyItemUrl
    where
      applyIId integration = applyValue (e ^. iId) integration iId
      applyName integration = applyValue (e ^. name) integration name
      applyProps integration = applyValue (e ^. props) integration props
      applyLogo integration = applyValue (e ^. logo) integration logo
      applyRequestMethod integration = applyValue (e ^. requestMethod) integration requestMethod
      applyRequestUrl integration = applyValue (e ^. requestUrl) integration requestUrl
      applyRequestHeaders integration = applyValue (e ^. requestHeaders) integration requestHeaders
      applyRequestBody integration = applyValue (e ^. requestBody) integration requestBody
      applyResponseListField integration = applyValue (e ^. responseListField) integration responseListField
      applyResponseIdField integration = applyValue (e ^. responseIdField) integration responseIdField
      applyResponseNameField integration = applyValue (e ^. responseNameField) integration responseNameField
      applyItemUrl integration = applyValue (e ^. itemUrl) integration itemUrl
