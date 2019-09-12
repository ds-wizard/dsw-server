module Service.Shacl.ShaclService where

import Control.Monad.Reader (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import DSW.SHACL.Transformation (shaclToEvents)

import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Constant.Component
import Localization.Messages.Public
import Model.Context.AppContext
import Model.Error.Error
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.KnowledgeModel.KnowledgeModelService
import Util.Helper (createHeeHelper)
import Util.Logger (logWarnU, msg)

createShaclPreview :: BS.ByteString -> AppContextM (Either AppError KnowledgeModelDTO)
createShaclPreview shacl = do
  heCreateEventsFromShacl shacl $ \events -> do
    km <- compileKnowledgeModel (fromDTOs events) Nothing []
    return . fmap toKnowledgeModelDTO $ km

createEventsFromShacl :: BS.ByteString -> AppContextM (Either AppError [EventDTO])
createEventsFromShacl shacl = do
  eEventsS <- liftIO $ shaclToEvents shacl
  case eEventsS of
    Right eventsS ->
      case eitherDecode eventsS of
        Right events -> return . Right $ events
        Left error -> do
          logWarnU $ msg _CMP_SERVICE ("Couln't deserialize events from Shacl convertor (" ++ (show error) ++ ")")
          return . Left . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ
    Left error -> do
      logWarnU $ msg _CMP_SERVICE ("Couln't convert shacl to events (" ++ (show error) ++ ")")
      return . Left . UserError $ _ERROR_API_COMMON__CANT_DESERIALIZE_OBJ

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateEventsFromShacl shacl callback = createHeeHelper (createEventsFromShacl shacl) callback
