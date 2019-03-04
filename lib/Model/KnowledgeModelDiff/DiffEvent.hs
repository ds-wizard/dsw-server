module Model.KnowledgeModelDiff.DiffEvent where

data DiffEvent
  = NodeAdded String
  | NodeDeleted String
  | NodeEdited String
  deriving (Show, Eq)