module Api.Resource.Migration.KnowledgeModel.MigrationStateDTO where

import GHC.Generics

import Model.Migration.KnowledgeModel.MigratorState

data MigrationStateDTO
  = RunningStateDTO
  | ConflictStateDTO Conflict
  | ErrorStateDTO
  | CompletedStateDTO
  deriving (Show, Eq, Generic)
