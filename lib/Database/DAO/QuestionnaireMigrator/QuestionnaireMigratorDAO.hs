module Database.DAO.QuestionnaireMigrator.QuestionnaireMigratorDAO where

import qualified Data.UUID as U
import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, fetch, findOne, insert, merge, save, select)

import Database.BSON.QuestionnaireMigrator.QuestionnaireMigratorState ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO
qtnmCollection = "questionnaireMigrations"

--createQuestionnaireMigratorState :: QuestionnaireMigratorStateCreateDTO -> AppContextM Value
--createQuestionnaireMigratorState state = do
--  let action = insert qtnmCollection (toBSON state)
--  runDB action
