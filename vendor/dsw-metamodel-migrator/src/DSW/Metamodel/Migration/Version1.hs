module DSW.Metamodel.Migration.Version1 where

import Data.Aeson
import Data.Either

import qualified DSW.Metamodel.Event.Version1 as V1
import qualified DSW.Metamodel.Event.Version2 as V2

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

migrateEventFieldDTO :: V1.EventFieldDTO a -> V2.EventFieldDTO a
migrateEventFieldDTO V1.NothingChangedDTO = V2.NothingChangedDTO
migrateEventFieldDTO (V1.ChangedValueDTO x) = (V2.ChangedValueDTO x)

migrateEventPathDTO :: V1.EventPathDTO -> Either String V2.EventPathDTO
migrateEventPathDTO = result2Either . fromJSON . toJSON

migrateEditKnowledgeModelEventDTO :: V1.EditKnowledgeModelEventDTO -> Either String V2.EditKnowledgeModelEventDTO
migrateEditKnowledgeModelEventDTO (V1.EditKnowledgeModelEventDTO {..}) = do
  newPath <- migrateEventPathDTO _editKnowledgeModelEventDTOPath
  return
    V2.EditKnowledgeModelEventDTO
    { V2._editKnowledgeModelEventDTOUuid = _editKnowledgeModelEventDTOUuid
    , V2._editKnowledgeModelEventDTOPath = newPath
    , V2._editKnowledgeModelEventDTOKmUuid = _editKnowledgeModelEventDTOKmUuid
    , V2._editKnowledgeModelEventDTOName = migrateEventFieldDTO _editKnowledgeModelEventDTOName
    , V2._editKnowledgeModelEventDTOChapterUuids = migrateEventFieldDTO _editKnowledgeModelEventDTOChapterUuids
    , V2._editKnowledgeModelEventDTOTagUuids = migrateEventFieldDTO _editKnowledgeModelEventDTOTagUuids
    , V2._editKnowledgeModelEventDTOIntegrationUuids = V2.NothingChangedDTO
    }

migrateEvent :: V1.EventDTO -> Either String V2.EventDTO
migrateEvent (V1.EditKnowledgeModelEventDTO' oldEvent) = do
  newEvent <- migrateEditKnowledgeModelEventDTO oldEvent
  return $ V2.EditKnowledgeModelEventDTO' newEvent
migrateEvent x = result2Either . fromJSON . toJSON $ x

migrateEventValue :: Value -> Either String Value
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- migrateEvent oldEvent
  return $ toJSON newEvent
