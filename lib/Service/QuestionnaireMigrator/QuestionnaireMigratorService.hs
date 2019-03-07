module Service.QuestionnaireMigrator.QuestionnaireMigratorService where

import Control.Lens ((^.))

import LensesConfig
import Model.Error.Error
import Model.Context.AppContext
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Model.Questionnaire.QuestionnaireState
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Database.DAO.QuestionnaireMigrator.QuestionnaireMigratorDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.Package.PackageDAO
import Service.KnowledgeModel.KnowledgeModelService
import Service.KnowledgeModelDiff.KnowledgeModelDiffService
import qualified Service.QuestionnaireMigrator.QuestionnaireMigratorMapper as QM

-- Creates new questionnaire migration from questionnaire id and target package id.
createQuestionnaireMigration :: String -> QuestionnaireMigratorStateCreateDTO -> AppContextM (Either AppError QuestionnaireMigratorStateDTO)
createQuestionnaireMigration qId qDto =
  -- TODO: Ensure there is no previous migration
  heFindQuestionnaireById qId $ \questionnaire ->
    heDiffKnowledgeModelsById (questionnaire ^. packageId) (qDto ^. targetPackageId) $ \kmDiff ->
      heFindPackageById (questionnaire ^. packageId) $ \package ->
        heCompileKnowledgeModel [] (Just $ questionnaire ^. packageId) (questionnaire ^. selectedTagUuids) $ \compiledKm -> do
          let state =
                QuestionnaireMigratorState
                  { _questionnaireMigratorStateQuestionnaire = questionnaire
                  , _questionnaireMigratorStateDiffKnowledgeModel = kmDiff ^. knowledgeModel
                  , _questionnaireMigratorStateTargetPackageId = qDto ^. targetPackageId
                  , _questionnaireMigratorStateDiffEvents = kmDiff ^. events
                  }
          createQuestionnaireMigratorState state
          -- TODO: Find current questionnaire state and remove import
          return . Right $ QM.toDTO state package compiledKm QSDefault

-- Creates backup for old questionnaire and moves migrated questionnaire to its place.
finishQuestionnaireMigration :: String -> Either AppError QuestionnaireMigratorStateDTO
finishQuestionnaireMigration = undefined

-- Returns current questionnaire migration state for given uuid.
getQuestionnaireMigration :: String -> AppContextM (Either AppError QuestionnaireMigratorStateDTO)
getQuestionnaireMigration qtnUuid =
  heFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid $ \state ->
    heFindQuestionnaireById qtnUuid $ \questionnaire ->
      heFindPackageById (state ^. targetPackageId) $ \package ->
        heCompileKnowledgeModel [] (Just $ questionnaire ^. packageId) (questionnaire ^. selectedTagUuids) $ \compiledKm ->
          -- TODO: Find current questionnaire state and remove import
          return . Right $ QM.toDTO state package compiledKm QSDefault

-- Cancels questionnaire migration for given uuid.
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
