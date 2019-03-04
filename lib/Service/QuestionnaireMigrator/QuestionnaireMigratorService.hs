module Service.QuestionnaireMigrator.QuestionnaireMigratorService where

import qualified Data.UUID as UUID
import Control.Lens ((^.))

import LensesConfig
import Model.Error.Error
import Model.Context.AppContext
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Database.DAO.QuestionnaireMigrator.QuestionnaireMigratorDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.Package.PackageDAO
import Service.KnowledgeModelDiff.KnowledgeModelDiffService
import qualified Service.QuestionnaireMigrator.QuestionnaireMigratorMapper as QM

-- Creates new questionnaire migration from questionnaire id and target package id.
createQuestionnaireMigration :: String -> QuestionnaireMigratorStateCreateDTO -> AppContextM (Either AppError QuestionnaireMigratorStateDTO)
createQuestionnaireMigration qId qDto =
  -- TODO: Ensure there is no previous migration
  heFindQuestionnaireById qId $ \questionnaire ->
    heDiffKnowledgeModelsById (questionnaire ^. packageId) (UUID.toString $ qDto ^. targetPackageId) $ \kmDiff ->
      heFindPackageById (questionnaire ^. packageId) $ \package -> do
        let state =
              QuestionnaireMigratorState
                { _questionnaireMigratorStateQuestionnaire = undefined
                , _questionnaireMigratorStateDiffKnowledgeModel = undefined
                , _questionnaireMigratorStateTargetPackageId = undefined
                }
        _ <- createQuestionnaireMigratorState state
        return . Right $ QM.toDTO state package

-- Creates backup for old questionnaire and moves migrated questionnaire to its place.
finishQuestionnaireMigration :: String -> Either AppError QuestionnaireMigratorStateDTO
finishQuestionnaireMigration = undefined

cancelQuestionnaireMigration :: String -> AppContextM (Maybe AppError)
cancelQuestionnaireMigration qtnUuid =
  hmFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid $ \_ -> do
    deleteQuestionnaireMigratorStateByQuestionnaireId qtnUuid
    return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------

heFindQuestionnaireMigratorStateByQuestionnaireId :: String -> (QuestionnaireMigratorState -> AppContextM (Either AppError a)) -> AppContextM (Either AppError a)
heFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid callback = do
  state <- findQuestionnaireMigratorStateByQuestionnaireId qtnUuid
  case state of
    Left error  -> return . Left $ error
    Right state -> callback state

hmFindQuestionnaireMigratorStateByQuestionnaireId :: String -> (QuestionnaireMigratorState -> AppContextM (Maybe AppError)) -> AppContextM (Maybe AppError)
hmFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid callback = do
  state <- findQuestionnaireMigratorStateByQuestionnaireId qtnUuid
  case state of
    Left error  -> return . Just $ error
    Right state -> callback state
