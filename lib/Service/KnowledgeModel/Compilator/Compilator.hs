module Service.KnowledgeModel.Compilator.Compilator
  ( runApplicator
  ) where

import Model.Error.Error
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.Compilator.EventApplicator.Answer ()
import Service.KnowledgeModel.Compilator.EventApplicator.Chapter ()
import Service.KnowledgeModel.Compilator.EventApplicator.Expert ()
import Service.KnowledgeModel.Compilator.EventApplicator.Integration ()
import Service.KnowledgeModel.Compilator.EventApplicator.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.EventApplicator.Question ()
import Service.KnowledgeModel.Compilator.EventApplicator.Reference ()
import Service.KnowledgeModel.Compilator.EventApplicator.Tag ()
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator

runApplicator :: Maybe KnowledgeModel -> [Event] -> Either AppError KnowledgeModel
runApplicator mKM events =
  -- TODO fix it to not to use undefined
  case mKM of
    Just km -> foldl foldEvent (Right km) events
    Nothing -> foldl foldEvent (Right undefined) events
  where
    foldEvent :: Either AppError KnowledgeModel -> Event -> Either AppError KnowledgeModel
    foldEvent eKm (AddKnowledgeModelEvent' e) = apply' e eKm
    foldEvent eKm (EditKnowledgeModelEvent' e) = apply' e eKm
    foldEvent eKm (AddChapterEvent' e) = apply' e eKm
    foldEvent eKm (EditChapterEvent' e) = apply' e eKm
    foldEvent eKm (DeleteChapterEvent' e) = apply' e eKm
    foldEvent eKm (AddQuestionEvent' e) = apply' e eKm
    foldEvent eKm (EditQuestionEvent' e) = apply' e eKm
    foldEvent eKm (DeleteQuestionEvent' e) = apply' e eKm
    foldEvent eKm (AddAnswerEvent' e) = apply' e eKm
    foldEvent eKm (EditAnswerEvent' e) = apply' e eKm
    foldEvent eKm (DeleteAnswerEvent' e) = apply' e eKm
    foldEvent eKm (AddExpertEvent' e) = apply' e eKm
    foldEvent eKm (EditExpertEvent' e) = apply' e eKm
    foldEvent eKm (DeleteExpertEvent' e) = apply' e eKm
    foldEvent eKm (AddReferenceEvent' e) = apply' e eKm
    foldEvent eKm (EditReferenceEvent' e) = apply' e eKm
    foldEvent eKm (DeleteReferenceEvent' e) = apply' e eKm
    foldEvent eKm (AddTagEvent' e) = apply' e eKm
    foldEvent eKm (EditTagEvent' e) = apply' e eKm
    foldEvent eKm (DeleteTagEvent' e) = apply' e eKm
    foldEvent eKm (AddIntegrationEvent' e) = apply' e eKm
    foldEvent eKm (EditIntegrationEvent' e) = apply' e eKm
    foldEvent eKm (DeleteIntegrationEvent' e) = apply' e eKm
    apply' _ (Left error) = Left error
    apply' event (Right km) = apply event km
