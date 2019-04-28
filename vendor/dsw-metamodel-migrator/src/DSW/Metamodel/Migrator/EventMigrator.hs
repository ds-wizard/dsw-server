module DSW.Metamodel.Migrator.EventMigrator
  ( migrate
  ) where

import Data.Aeson

import qualified DSW.Metamodel.Migration.Version1 as Migration1

type Version = Int

migrate :: Version -> Version -> Value -> Either String Value
migrate vSrc vDst input
  | vSrc > vDst = Left "Downgrade not supported"
  | vSrc == vDst = Right input
  | vSrc == 1 && vDst == 2 = Migration1.migrateEventValue input
  | otherwise = Left "Unsupported metamodel version"
