module Api.Handler.QuestionnaireMigrator.QuestionnaireMigratorHandler where

import Network.HTTP.Types.Status (noContent204)
import Web.Scotty.Trans (addHeader, json, param, raw, status)

import Api.Handler.Common
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO ()
import Service.QuestionnaireMigrator.QuestionnaireMigratorService

-- Endpoint for creating questionnaire migration to newer (target) knowledgemodel.
--postQuestionaireMigrationsA :: Endpoint
--postQuestionaireMigrationsA =
--  checkPermission "QTN_PERM" $
--  getAuthServiceExecutor $ \runInAuthService ->
--    getReqDto $ \reqDto -> do
--      qtnUuid <- param "qtnUuid"
--      eitherMigrationDto <- runInAuthService $ createQuestionnaireMigration qtnUuid reqDto
--      case eitherDtos of
--        Right dtos -> json dtos
--        Left error -> sendError error

-- Endpoint for canceling questionnaire migration.
deleteQuestionnaireMigrationsCurrentA :: Endpoint
deleteQuestionnaireMigrationsCurrentA = undefined
  checkPermission "QTN_PERM" $
    getAuthServiceExecutor $ \runInAuthService -> do
      qtnUuid <- param "questionnaireUuid"
      result  <- runInAuthService $ cancelQuestionnaireMigration qtnUuid
      case result of
        Nothing    -> status noContent204
        Just error -> sendError error

