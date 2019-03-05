module Service.KnowledgeModelDiff.KnowledgeModelDiffService
  ( diffKnowledgeModelsById
  , heDiffKnowledgeModelsById
  ) where

import Model.Error.Error
import Model.Context.AppContext
import Model.Event.Event
import Model.KnowledgeModelDiff.KnowledgeModelDiff

import Service.Package.PackageService
  ( heGetAllPreviousEventsSincePackageId
  , heGetAllPreviousEventsSincePackageIdAndUntilPackageId
  )
import Service.KnowledgeModel.KnowledgeModelService (heCompileKnowledgeModel)
import Service.Migration.KnowledgeModel.Applicator.Applicator
-- Creates new knowledgemodel-like diff tree and diff events between
-- an old knowledgemodel and a new knowledgemodel.
diffKnowledgeModelsById :: String -> String -> AppContextM (Either AppError KnowledgeModelDiff)
diffKnowledgeModelsById oldKmId newKmId =
  -- TODO: Validate input data here
  heGetAllPreviousEventsSincePackageId oldKmId $ \oldKmEvents ->
    heCompileKnowledgeModel oldKmEvents Nothing [] $ \oldKm ->
      heGetAllPreviousEventsSincePackageIdAndUntilPackageId newKmId oldKmId $ \newKmEvents ->
        case runDiffApplicator (Just oldKm) newKmEvents of
          Left error -> return . Left $ error
          Right km   -> return . Right $ KnowledgeModelDiff
            { _knowledgeModelDiffKnowledgeModel = km
            , _knowledgeModelDiffEvents = cleanUpDiffEvents newKmEvents
            }

-- Cleans up redundant diff events and preservers first edit event only.
cleanUpDiffEvents :: [Event] -> [Event]
cleanUpDiffEvents = id

-- --------------------------------
-- HELPERS
-- --------------------------------

-- Helper knowledgemodel diffing function. Creates diff between old knowledgemodel
-- and new knowledgemodel. Calls given callback on success.
heDiffKnowledgeModelsById :: String -> String -> (KnowledgeModelDiff -> AppContextM (Either AppError a)) -> AppContextM (Either AppError a)
heDiffKnowledgeModelsById oldKmId newKmId callback = do
  eitherDiff <- diffKnowledgeModelsById oldKmId newKmId
  case eitherDiff of
    Left error   -> return . Left $ error
    Right kmDiff -> callback kmDiff