module Service.KnowledgeModelDiff.KnowledgeModelDiff
( diffKnowledgemodelsById ) where

import Model.Error.Error
import Model.Context.AppContext
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Package.PackageService
  ( heGetAllPreviousEventsSincePackageId
  , heGetAllPreviousEventsSincePackageIdAndUntilPackageId
  )
import Service.KnowledgeModel.KnowledgeModelApplicator (heCreateKnowledgeModel)

data DiffEvent = DiffEvent
data KnowledgeModelDiff = KnowledgeModelDiff KnowledgeModel [DiffEvent]

-- Creates new knowledgemodel-like diff tree and diff events based on given knowledge model uuids
diffKnowledgemodelsById :: String -> String -> AppContextM (Either AppError KnowledgeModelDiff)
diffKnowledgemodelsById oldKmId newKmId = do
  -- TODO: Validate input data here
  heGetAllPreviousEventsSincePackageId oldKmId $ \oldKmEvents ->
    heCreateKnowledgeModel oldKmEvents $ \oldKm ->
      heGetAllPreviousEventsSincePackageIdAndUntilPackageId newKmId oldKmId $ \newKmEvents ->
        createKnowledgemodelDiff oldKm newKmEvents

-- Creates knowledgemodel diff tree by applying events on compiled knowledgemodel.
-- Note: The events must logically follow the knowledgemodel
createKnowledgemodelDiff :: KnowledgeModel -> [Event] -> AppContextM (Either AppError KnowledgeModelDiff)
createKnowledgemodelDiff km events = undefined

-- Helpers
