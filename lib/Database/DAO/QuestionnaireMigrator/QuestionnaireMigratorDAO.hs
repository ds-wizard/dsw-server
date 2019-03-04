module Database.DAO.QuestionnaireMigrator.QuestionnaireMigratorDAO
  ( findQuestionnaireMigratorStateByQuestionnaireId
  , deleteQuestionnaireMigratorStateByQuestionnaireId
  ) where

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
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO

qtnmCollection = "questionnaireMigrations"

--createQuestionnaireMigratorState :: QuestionnaireMigratorStateCreateDTO -> AppContextM Value
--createQuestionnaireMigratorState state = do
--  let action = insert qtnmCollection (toBSON state)
--  runDB action

findQuestionnaireMigratorStateByQuestionnaireId :: String -> AppContextM (Either AppError QuestionnaireMigratorState)
findQuestionnaireMigratorStateByQuestionnaireId qtnUuid = do
  let action = findOne $ select ["questionnaireUuid" =: qtnUuid] qtnmCollection
  maybeState <- runDB action
  return . deserializeMaybeEntity $ maybeState

deleteQuestionnaireMigratorStateByQuestionnaireId :: String -> AppContextM ()
deleteQuestionnaireMigratorStateByQuestionnaireId qtnUuid = do
  let action = delete $ select ["questionnaireUuid" =: qtnUuid] qtnmCollection
  runDB action
