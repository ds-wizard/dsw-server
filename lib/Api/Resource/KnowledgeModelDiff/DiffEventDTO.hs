module Api.Resource.KnowledgeModelDiff.DiffEventDTO where

import qualified Model.KnowledgeModelDiff.DiffEvent as DE

data DiffEventDTO
  = NodeAdded String
  | NodeDeleted String
  | NodeEdited String

-- Converts DiffEvent DTO to model object
fromDTO :: DiffEventDTO-> DE.DiffEvent
fromDTO (NodeAdded path)   = DE.NodeAdded path
fromDTO (NodeDeleted path) = DE.NodeDeleted path
fromDTO (NodeEdited path)  = DE.NodeEdited path

-- Converts DiffEvent model object to DTO
toDTO :: DE.DiffEvent -> DiffEventDTO
toDTO (DE.NodeAdded path)   = NodeAdded path
toDTO (DE.NodeDeleted path) = NodeDeleted path
toDTO (DE.NodeEdited path)  = NodeEdited path
