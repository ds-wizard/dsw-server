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
import Model.KnowledgeModelDiff.DiffEvent
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
        case runDiffApplicator oldKm newKmEvents of
          Left error -> return . Left $ error
          Right km   -> return . Right $ KnowledgeModelDiff
            { _knowledgeModelDiffKnowledgeModel = km
            , _knowledgeModelDiffEvents = createDiffEvents newKmEvents
            }

-- Runs custom knowledgemodel migration and omits delete events (so deleted paths are still available
-- for diff events)
runDiffApplicator :: KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
runDiffApplicator km events = runApplicator (Just km) editedEvents
  where editedEvents = filter isNotDeleteEvent events
        isNotDeleteEvent (DeleteChapterEvent' _)   = False
        isNotDeleteEvent (DeleteQuestionEvent' _ ) = False
        isNotDeleteEvent (DeleteAnswerEvent' _)    = False
        isNotDeleteEvent (DeleteExpertEvent' _)    = False
        isNotDeleteEvent (DeleteReferenceEvent' _) = False
        isNotDeleteEvent _                         = True

createDiffEvents :: [Event] -> [DiffEvent]
createDiffEvents = map convertKmEventToDiffEvent

convertKmEventToDiffEvent :: Event -> DiffEvent
convertKmEventToDiffEvent (AddKnowledgeModelEvent' e) = NodeAdded ""
convertKmEventToDiffEvent (EditKnowledgeModelEvent' e) = NodeEdited ""
convertKmEventToDiffEvent (AddChapterEvent' _) = NodeAdded ""
convertKmEventToDiffEvent (EditChapterEvent' _) = NodeEdited ""
convertKmEventToDiffEvent (DeleteChapterEvent' _) = NodeEdited ""
convertKmEventToDiffEvent (AddQuestionEvent' _) = NodeAdded ""
convertKmEventToDiffEvent (EditQuestionEvent' _) = NodeEdited ""
convertKmEventToDiffEvent (DeleteQuestionEvent' _) = NodeDeleted ""
convertKmEventToDiffEvent (AddAnswerEvent' _) = NodeAdded ""
convertKmEventToDiffEvent (EditAnswerEvent' _) = NodeEdited ""
convertKmEventToDiffEvent (DeleteAnswerEvent' _) = NodeDeleted ""
convertKmEventToDiffEvent (AddExpertEvent' _) = NodeAdded ""
convertKmEventToDiffEvent (EditExpertEvent' _) = NodeEdited ""
convertKmEventToDiffEvent (DeleteExpertEvent' _) = NodeDeleted ""
convertKmEventToDiffEvent (AddReferenceEvent' _) = NodeAdded ""
convertKmEventToDiffEvent (EditReferenceEvent' _) = NodeEdited ""
convertKmEventToDiffEvent (DeleteReferenceEvent' _) = NodeDeleted ""

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