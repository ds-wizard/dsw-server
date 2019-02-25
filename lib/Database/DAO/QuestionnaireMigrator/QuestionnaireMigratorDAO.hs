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
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

qtnmCollection = "questionnaireMigrations"

findQuestionnaireMigratorStateByQuestionnaireUuid :: U.UUID -> AppContextM (Either AppError QuestionnaireMigratorState)
findQuestionnaireMigratorStateByQuestionnaireUuid uuid = do
  let action = findOne $ select ["questionnaireUuid" =: uuid] qtnmCollection
  maybeState <- runDB action
  return . deserializeMaybeEntity $ maybeState

insertQuestionnaireMigratorState :: QuestionnaireMigratorMigrationState -> AppContextM Value
insertQuestionnaireMigratorState state = do
  let action = insert qtnmCollection (toBSON state)
  runDB action
