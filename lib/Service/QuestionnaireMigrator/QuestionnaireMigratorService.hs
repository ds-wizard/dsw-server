module Service.QuestionnaireMigrator.QuestionnaireMigratorService
  ( createQuestionnaireMigration
  , finishQuestionnaireMigration
  , getQuestionnaireMigration
  , cancelQuestionnaireMigration
  , getQuestionnaireState
  , heGetQuestionnaireState
  ) where

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
import Service.Package.PackageService
import qualified Service.QuestionnaireMigrator.QuestionnaireMigratorMapper as QM

-- Creates new questionnaire migration from questionnaire id and target package id.
createQuestionnaireMigration :: String -> QuestionnaireMigratorStateCreateDTO -> AppContextM (Either AppError QuestionnaireMigratorStateDTO)
createQuestionnaireMigration qId qDto =
  -- TODO: Ensure there is no previous migration
  heFindQuestionnaireById qId $ \questionnaire ->
    heDiffKnowledgeModelsById (questionnaire ^. packageId) (qDto ^. targetPackageId) $ \kmDiff ->
      heFindPackageById (questionnaire ^. packageId) $ \package ->
        heCompileKnowledgeModel [] (Just $ questionnaire ^. packageId) (questionnaire ^. selectedTagUuids) $ \compiledKm ->
          heGetQuestionnaireState qId (qDto ^. targetPackageId) $ \qtnState -> do
            let state =
                  QuestionnaireMigratorState
                    { _questionnaireMigratorStateQuestionnaire = questionnaire
                    , _questionnaireMigratorStateTargetPackageId = qDto ^. targetPackageId
                    }
            createQuestionnaireMigratorState state
            return . Right $ QM.toDTO state kmDiff package compiledKm qtnState

-- Creates backup for old questionnaire and moves migrated questionnaire to its place.
finishQuestionnaireMigration :: String -> Either AppError QuestionnaireMigratorStateDTO
finishQuestionnaireMigration = undefined

-- Returns current questionnaire migration state for given uuid.
getQuestionnaireMigration :: String -> AppContextM (Either AppError QuestionnaireMigratorStateDTO)
getQuestionnaireMigration qtnUuid =
  heFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid $ \mState ->
    heFindQuestionnaireById qtnUuid $ \questionnaire ->
      heFindPackageById (mState ^. targetPackageId) $ \package ->
        heCompileKnowledgeModel [] (Just $ questionnaire ^. packageId) (questionnaire ^. selectedTagUuids) $ \compiledKm -> do
          let prevPkgId = questionnaire ^. packageId
          let targetPkgId = mState ^. targetPackageId
          heDiffKnowledgeModelsById prevPkgId targetPkgId $ \kmDiff ->
            heGetQuestionnaireState qtnUuid (mState ^. targetPackageId) $ \qtnState ->
              return . Right $ QM.toDTO mState kmDiff package compiledKm qtnState

-- Cancels questionnaire migration for given uuid.
cancelQuestionnaireMigration :: String -> AppContextM (Maybe AppError)
cancelQuestionnaireMigration qtnUuid =
  hmFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid $ \_ -> do
    deleteQuestionnaireMigratorStateByQuestionnaireId qtnUuid
    return Nothing

-- Determines current questionnaire state based on questionnaire and package id.
getQuestionnaireState :: String -> String -> AppContextM (Either AppError QuestionnaireState)
getQuestionnaireState qtnUuid pkgId =
  heIsQuestionnaireStateIsMigrating qtnUuid $ \_ ->
    heQuestionnaireStateIsOutdated pkgId $ \_ ->
      return . Right $ QSDefault

-- --------------------------------
-- HELPERS
-- --------------------------------

-- Determines current questionnaire state based on questionnaire and package id.
heGetQuestionnaireState :: String -> String -> ((QuestionnaireState) -> AppContextM (Either AppError a)) -> AppContextM (Either AppError a)
heGetQuestionnaireState qtnUuid pkgId callback = do
  eState <- getQuestionnaireState qtnUuid pkgId
  case eState of
    Left error  -> return . Left $ error
    Right state -> callback state

-- Checks whether the questionnaire state is currently migrating. Calls callback otherwise.
heIsQuestionnaireStateIsMigrating :: String -> (() -> AppContextM (Either AppError QuestionnaireState)) -> AppContextM (Either AppError QuestionnaireState)
heIsQuestionnaireStateIsMigrating qtnUuid elseCallback = do
  eMigrationState <- findQuestionnaireMigratorStateByQuestionnaireId qtnUuid
  case eMigrationState of
    Left (NotExistsError _) -> elseCallback ()
    Left error              -> return . Left $ error
    otherwise               -> return . Right $ QSMigrating

-- Checks whether the questionnaire state is currently outdated based on package id. Calls callback otherwise.
heQuestionnaireStateIsOutdated :: String -> (() -> AppContextM (Either AppError QuestionnaireState)) -> AppContextM (Either AppError QuestionnaireState)
heQuestionnaireStateIsOutdated pkgId elseCallback = do
  eNewerPackages <- getNewerPackages $ pkgId
  case eNewerPackages of
    Left error -> return . Left $ error
    Right pkgs -> case Prelude.length pkgs of
      0         -> elseCallback ()
      otherwise -> return . Right $ QSOutdated

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
