module Service.KnowledgeModel.Compilator.Modifier.Expert where

import Control.Lens ((^.))

import LensesConfig
import Model.Event.Expert.ExpertEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddExpertEvent Expert where
  createEntity e = Expert {_expertUuid = e ^. entityUuid, _expertName = e ^. name, _expertEmail = e ^. email}

instance EditEntity EditExpertEvent Expert where
  editEntity e = applyEmail . applyName
    where
      applyName exp = applyValue (e ^. name) exp name
      applyEmail exp = applyValue (e ^. email) exp email
