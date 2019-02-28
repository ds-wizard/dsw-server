module Service.KnowledgeModelDiff.KnowledgeModelDiff
  ( diffKnowledgemodelsById
  ) where

import Model.Error.Error
import Model.Context.AppContext
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Package.PackageService
  ( heGetAllPreviousEventsSincePackageId
  , heGetAllPreviousEventsSincePackageIdAndUntilPackageId
  )
import Service.KnowledgeModel.KnowledgeModelApplicator (heCreateKnowledgeModel)
import Service.Migrator.Applicator.Applicator

data DiffEvent
  = NodeAdded String
  | NodeDeleted String
  | NodeEdited String

data KnowledgeModelDiff = KnowledgeModelDiff
  { _knowledgeModelDiffKnowledgeModel :: KnowledgeModel
  , _knowledgeModelDiffEvents :: [DiffEvent]
  }

-- Creates new knowledgemodel-like diff tree and diff events based on given knowledge model uuids
diffKnowledgemodelsById :: String -> String -> AppContextM (Either AppError KnowledgeModelDiff)
diffKnowledgemodelsById oldKmId newKmId =
  -- TODO: Validate input data here
  heGetAllPreviousEventsSincePackageId oldKmId $ \oldKmEvents ->
    heCreateKnowledgeModel oldKmEvents $ \oldKm ->
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
createDiffEvents []     = []
createDiffEvents (e:es) = [sanitizeEvent e] ++ (createDiffEvents es)
  where sanitizeEvent _ = NodeEdited ""
