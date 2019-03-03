module Service.QuestionnaireMigrator.QuestionnaireMigratorService where

import qualified Data.UUID as UUID
import Control.Lens ((^.))

import LensesConfig
import Model.Error.Error
import Model.Context.AppContext
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Service.KnowledgeModelDiff.KnowledgeModelDiffService

-- Remove latter
import Model.QuestionnaireMigrator.QuestionnaireMigratorState

-- Creates new questionnaire migration from questionnaire id and target package id.
createQuestionnaireMigration :: String -> QuestionnaireMigratorStateCreateDTO -> AppContextM (Either AppError QuestionnaireMigratorState)
createQuestionnaireMigration qId qDto =
  -- TODO: Ensure there is no previous migration
  heFindQuestionnaireById qId $ \questionnaire ->
    heDiffKnowledgeModelsById (questionnaire ^. packageId) (UUID.toString $ qDto ^. targetPackageId) $ \kmDiff ->
      return . Right $ QuestionnaireMigratorState
        { _questionnaireMigratorStateQuestionnaire      = questionnaire
        , _questionnaireMigratorStateDiffKnowledgeModel = kmDiff ^. knowledgeModel
        , _questionnaireMigratorStateTargetPackageId    = qDto ^. targetPackageId
        }

-- Creates backup for old questionnaire and moves migrated questionnaire to its place.
finishQuestionnaireMigration :: String -> Either AppError QuestionnaireMigratorStateDTO
finishQuestionnaireMigration = undefined
