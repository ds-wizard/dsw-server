module Service.KnowledgeModelDiff.KnowledgeModelDiffService
  ( diffKnowledgeModelsById
  , heDiffKnowledgeModelsById
  ) where

import qualified Data.UUID as U
import Control.Lens ((^.))

import LensesConfig
import Model.Error.Error
import Model.Context.AppContext
import Model.Event.Event
import Model.Event.EventPath
import Model.KnowledgeModelDiff.KnowledgeModelDiff
import Model.KnowledgeModel.KnowledgeModel

import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Expert.ExpertEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent

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

-- Cleans up diff redudant diff events
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