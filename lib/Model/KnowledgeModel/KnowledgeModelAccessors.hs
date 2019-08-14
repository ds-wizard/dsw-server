module Model.KnowledgeModel.KnowledgeModelAccessors where

import Control.Lens
import Data.List
import qualified Data.Map as M
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel

class EntityAccesors a where
  getEntityUuid :: a -> U.UUID

instance EntityAccesors KnowledgeModel where
  getEntityUuid entity = entity ^. uuid

instance EntityAccesors Chapter where
  getEntityUuid entity = entity ^. uuid

instance EntityAccesors Question where
  getEntityUuid (OptionsQuestion' entity) = entity ^. uuid
  getEntityUuid (ListQuestion' entity) = entity ^. uuid
  getEntityUuid (ValueQuestion' entity) = entity ^. uuid
  getEntityUuid (IntegrationQuestion' entity) = entity ^. uuid

instance EntityAccesors Answer where
  getEntityUuid entity = entity ^. uuid

instance EntityAccesors Expert where
  getEntityUuid entity = entity ^. uuid

instance EntityAccesors Reference where
  getEntityUuid (ResourcePageReference' entity) = entity ^. uuid
  getEntityUuid (URLReference' entity) = entity ^. uuid
  getEntityUuid (CrossReference' entity) = entity ^. uuid

instance EntityAccesors Tag where
  getEntityUuid entity = entity ^. uuid

instance EntityAccesors Integration where
  getEntityUuid entity = entity ^. uuid
