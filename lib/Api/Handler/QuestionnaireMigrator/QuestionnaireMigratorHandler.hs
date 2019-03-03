module Api.Handler.QuestionnaireMigrator.QuestionnaireMigratorHandler where

import Web.Scotty.Trans (addHeader, json, param, raw, status)

import Api.Handler.Common
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO ()
import Service.KnowledgeModelDiff.KnowledgeModelDiffService

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
