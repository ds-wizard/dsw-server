module Api.Handler.QuestionnaireMigrator.QuestionnaireMigratorHandler where

import Network.HTTP.Types.Status (noContent204, created201)
import Web.Scotty.Trans (param, status)

import Api.Handler.Common
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateJM ()
import Service.QuestionnaireMigrator.QuestionnaireMigratorService

-- Endpoint for creating questionnaire migration to newer (target) knowledgemodel.
postQuestionnaireMigrationsCurrentA :: Endpoint
postQuestionnaireMigrationsCurrentA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService ->
    getReqDto $ \reqDto -> do
      qtnUuid <- param "qtnUuid"
      eitherMigrationDto <- runInAuthService $ createQuestionnaireMigration qtnUuid reqDto
      case eitherMigrationDto of
        Right _ -> status created201
        Left error -> sendError error

-- Endpoint for canceling questionnaire migration.
deleteQuestionnaireMigrationsCurrentA :: Endpoint
deleteQuestionnaireMigrationsCurrentA =
  checkPermission "QTN_PERM" $
    getAuthServiceExecutor $ \runInAuthService -> do
      qtnUuid <- param "questionnaireUuid"
      result  <- runInAuthService $ cancelQuestionnaireMigration qtnUuid
      case result of
        Nothing    -> status noContent204
        Just error -> sendError error
