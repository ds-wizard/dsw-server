module Database.Connection where

import Control.Lens ((^.))
import Data.Text
import Database.Persist.MongoDB
import Network

import LensesConfig

createDatabaseConnectionPool dswConfig = do
  createMongoDBPool dbName dbHost dbPort dbCred 1 1 1
  where
    appConfigDatabase = dswConfig ^. databaseConfig
    dbHost = appConfigDatabase ^. host
    dbPort = PortNumber (fromInteger (appConfigDatabase ^. port) :: PortNumber) :: PortID
    dbName = pack (appConfigDatabase ^. databaseName)
    dbCred = mkDBCred appConfigDatabase
    mkDBCred appConfigDatabase =
      if appConfigDatabase ^. authEnabled
        then Just $ MongoAuth (pack $ appConfigDatabase ^. username) (pack $ appConfigDatabase ^. password)
        else Nothing
