module Service.QuestionnaireMigrator.QuestionnaireMigratorService
  ( createQuestionnaireMigration
  , finishQuestionnaireMigration
  , getQuestionnaireMigration
  , cancelQuestionnaireMigration
  , getQuestionnaireState
  , heGetQuestionnaireState
  , resolveQuestionnaireQuestionChange
  , completeQuestionnairemigration
  ) where

import Control.Lens ((^.), (&), (.~))
import Control.Monad.Reader (liftIO)
import Data.List (intercalate)

import LensesConfig
import Model.Error.Error
import Model.Context.AppContext
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Model.Questionnaire.QuestionnaireState
import Model.Questionnaire.QuestionFlag
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireReply (Reply)
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Api.Resource.Questionnaire.QuestionFlagDTO
import Database.DAO.QuestionnaireMigrator.QuestionnaireMigratorDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.Package.PackageDAO
import Service.KnowledgeModel.KnowledgeModelService
import Service.KnowledgeModelDiff.KnowledgeModelDiffService
import Service.Package.PackageService
import Service.Questionnaire.QuestionFlagMapper
import qualified Service.QuestionnaireMigrator.QuestionnaireMigratorMapper as QM
import Util.Uuid

-- Creates new questionnaire migration from questionnaire id and target package id.
createQuestionnaireMigration :: String -> QuestionnaireMigratorStateCreateDTO -> AppContextM (Either AppError QuestionnaireMigratorStateDTO)
createQuestionnaireMigration qId qDto =
  validateIfTargetPackageVersionIsHigher
  heGuardQuestionnaireMigrationNotExist qId $
    heFindQuestionnaireById qId $ \questionnaire ->
      heDiffKnowledgeModelsById (questionnaire ^. packageId) (qDto ^. targetPackageId) $ \kmDiff ->
        heFindPackageById (questionnaire ^. packageId) $ \package ->
          heCompileKnowledgeModel [] (Just $ questionnaire ^. packageId) (questionnaire ^. selectedTagUuids) $ \compiledKm -> do
            let state =
                  QuestionnaireMigratorState
                    { _questionnaireMigratorStateQuestionnaire = questionnaire
                    , _questionnaireMigratorStateTargetPackageId = qDto ^. targetPackageId
                    }
            createQuestionnaireMigratorState state
            return . Right $ QM.toDTO state kmDiff package compiledKm QSMigrating

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
  heIsQuestionnaireStateIsMigrating qtnUuid $
    heQuestionnaireStateIsOutdated pkgId $
      return . Right $ QSDefault

-- Completes the migration of questionnaire from old version of knowledge model to new version
completeQuestionnairemigration :: String -> AppContextM (Maybe AppError)
completeQuestionnairemigration qtnUuid =
  hmFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid $ \state ->
    hmGetAllPreviousEventsSincePackageId (state ^. targetPackageId) $ \events -> do
      let oldQuestionnaire = state ^. questionnaire
          newReplies = migrateQuestionnaireReplies $ oldQuestionnaire ^. replies
          newQuestionFlags = migrateQuestionnaireQuestionFlags $ oldQuestionnaire ^. questionFlags
          newPackageId = state ^. targetPackageId
      newUuid <- liftIO generateUuid
      insertQuestionnaire Questionnaire
        { _questionnaireUuid = newUuid
        , _questionnaireName = oldQuestionnaire ^. name
        , _questionnaireLevel = oldQuestionnaire ^. level
        , _questionnairePrivate = oldQuestionnaire ^. private
        , _questionnairePackageId = newPackageId
        , _questionnaireSelectedTagUuids = oldQuestionnaire ^. selectedTagUuids
        , _questionnaireOwnerUuid = oldQuestionnaire ^. ownerUuid
        , _questionnaireReplies = newReplies
        , _questionnaireCreatedAt = oldQuestionnaire ^. createdAt
        , _questionnaireUpdatedAt = oldQuestionnaire ^. updatedAt
        , _questionnaireQuestionFlags = newQuestionFlags
        }
      deleteQuestionnaireMigratorStateByQuestionnaireId qtnUuid
      return Nothing

heGuardQuestionnaireMigrationNotExist :: String -> AppContextM (Either AppError a) -> AppContextM (Either AppError a)
heGuardQuestionnaireMigrationNotExist qtnUuid callback = do
  state <- findQuestionnaireMigratorStateByQuestionnaireId qtnUuid
  case state of
    Left (NotExistsError _) -> callback
    Left error              -> return . Left $ error
    otherwise               -> return . Left $ MigratorError "Questionnaire is already being migrated."

-- --------------------------------
-- Flags
-- --------------------------------

resolveQuestionnaireQuestionChange :: String -> QuestionFlagsDTO -> AppContextM (Maybe AppError)
resolveQuestionnaireQuestionChange qtnUuid qtnFlag =
  hmFindQuestionnaireMigratorStateByQuestionnaireId qtnUuid $ \mState -> do
    let flags = filter (ignoreFlagsAtPath (qtnFlag ^. questionPath)) $ (mState ^. questionnaire) ^. questionFlags
        newFlag = fromQuestionFlagDTO qtnFlag
        updatedFlags = flags ++ [newFlag]
        updatedState = mState & questionnaire . questionFlags .~ updatedFlags
    updateQuestionnareMigratorStateByQuestionnaireId updatedState
    return Nothing
    where ignoreFlagsAtPath :: [String] -> QuestionFlags -> Bool
          ignoreFlagsAtPath path flags = path /= flags ^. questionPath

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
heIsQuestionnaireStateIsMigrating :: String -> AppContextM (Either AppError QuestionnaireState) -> AppContextM (Either AppError QuestionnaireState)
heIsQuestionnaireStateIsMigrating qtnUuid elseCallback = do
  eMigrationState <- findQuestionnaireMigratorStateByQuestionnaireId qtnUuid
  case eMigrationState of
    Left (NotExistsError _) -> elseCallback
    Left error              -> return . Left $ error
    otherwise               -> return . Right $ QSMigrating

-- Checks whether the questionnaire state is currently outdated based on package id. Calls callback otherwise.
heQuestionnaireStateIsOutdated :: String -> AppContextM (Either AppError QuestionnaireState) -> AppContextM (Either AppError QuestionnaireState)
heQuestionnaireStateIsOutdated pkgId elseCallback = do
  eNewerPackages <- getNewerPackages $ pkgId
  case eNewerPackages of
    Left error -> return . Left $ error
    Right pkgs -> case Prelude.length pkgs of
      0         -> elseCallback
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

migrateQuestionnaireReplies :: [Reply] -> [Reply]
migrateQuestionnaireReplies = id

migrateQuestionnaireQuestionFlags :: [QuestionFlags] -> [QuestionFlags]
migrateQuestionnaireQuestionFlags = id
