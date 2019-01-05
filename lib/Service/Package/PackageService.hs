module Service.Package.PackageService
  ( getPackagesFiltered
  , getSimplePackagesFiltered
  , getPackageById
  , getPackageWithEventsById
  , getTheNewestPackageByOrganizationIdAndKmId
  , getSeriesOfPackages
  , getAllPreviousEventsSincePackageId
  , getAllPreviousEventsSincePackageIdAndUntilPackageId
  , getEventsForBranchUuid
  , getNewerPackages
  , createPackage
  , createPackageFromKMC
  , deletePackagesByQueryParams
  , deletePackage
  -- Helpers
  , heGetSeriesOfPackages
  , heGetAllPreviousEventsSincePackageId
  , hmGetAllPreviousEventsSincePackageId
  , heGetAllPreviousEventsSincePackageIdAndUntilPackageId
  , heGetEventsForBranchUuid
  , heGetNewerPackages
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID as U

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageWithEventsDTO
import Api.Resource.Version.VersionDTO
import Database.DAO.Branch.BranchDAO
import Database.DAO.Event.EventDAO
import Database.DAO.Migrator.MigratorDAO
import Database.DAO.Package.PackageDAO
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Event.Event
import Model.Package.Package
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Organization.OrganizationService
import Service.Package.PackageMapper
import Service.Package.PackageUtils
import Service.Package.PackageValidation

getPackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageDTO])
getPackagesFiltered queryParams =
  heFindPackagesFiltered queryParams $ \packages -> return . Right . fmap packageToDTO $ packages

getSimplePackagesFiltered :: [(Text, Text)] -> AppContextM (Either AppError [PackageSimpleDTO])
getSimplePackagesFiltered queryParams = do
  heFindPackagesFiltered queryParams $ \packages -> do
    let uniquePackages = foldl addIfUnique [] packages
    return . Right $ uniquePackages
  where
    addIfUnique :: [PackageSimpleDTO] -> Package -> [PackageSimpleDTO]
    addIfUnique packageDtos newPackage =
      case isAlreadyInArray packageDtos newPackage of
        (Just packageDto) ->
          let withoutDesiredPackage = delete packageDto packageDtos
              updatedPackageDto = computeLatestVersion packageDto newPackage
          in withoutDesiredPackage ++ [updatedPackageDto]
        Nothing -> packageDtos ++ [packageToSimpleDTO newPackage]
    isAlreadyInArray :: [PackageSimpleDTO] -> Package -> Maybe PackageSimpleDTO
    isAlreadyInArray packageDtos newPackage =
      find
        (\pkg -> equalSameKmId (newPackage ^. kmId) pkg && equalSameOrganizationId (newPackage ^. organizationId) pkg)
        packageDtos
    hasSameKmId :: Package -> Package -> Bool
    hasSameKmId pkg1 pkg2 = pkg1 ^. kmId == pkg2 ^. kmId
    equalSameKmId :: String -> PackageSimpleDTO -> Bool
    equalSameKmId pkgKmId pkg = pkgKmId == pkg ^. kmId
    equalSameOrganizationId :: String -> PackageSimpleDTO -> Bool
    equalSameOrganizationId pkgOrganizationId pkg = pkgOrganizationId == pkg ^. organizationId
    computeLatestVersion :: PackageSimpleDTO -> Package -> PackageSimpleDTO
    computeLatestVersion packageDto newPackage =
      let originalVersion = packageDto ^. latestVersion
          newVersion = newPackage ^. version
      in if isNothing $ validateIsVersionHigher newVersion originalVersion
           then packageDto & latestVersion .~ newVersion
           else packageDto

getPackageById :: String -> AppContextM (Either AppError PackageDTO)
getPackageById pkgId = heFindPackageById pkgId $ \package -> return . Right . packageToDTO $ package

getPackageWithEventsById :: String -> AppContextM (Either AppError PackageWithEventsDTO)
getPackageWithEventsById pkgId =
  heFindPackageWithEventsById pkgId $ \package -> return . Right . packageWithEventsToDTOWithEvents $ package

getTheNewestPackageByOrganizationIdAndKmId :: String -> String -> AppContextM (Either AppError (Maybe Package))
getTheNewestPackageByOrganizationIdAndKmId organizationId kmId =
  heFindPackagesByOrganizationIdAndKmId organizationId kmId $ \packages -> do
    if length packages == 0
      then return . Right $ Nothing
      else do
        let sorted = sortPackagesByVersion packages
        return . Right . Just . head $ sorted

getSeriesOfPackages :: String -> AppContextM (Either AppError [PackageWithEvents])
getSeriesOfPackages pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. parentPackageId of
      Just parentPkgId ->
        heGetSeriesOfPackages parentPkgId $ \parentPackages -> return . Right $ parentPackages ++ [package]
      Nothing -> return . Right $ [package]

getAllPreviousEventsSincePackageId :: String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageId pkgId =
  heFindPackageWithEventsById pkgId $ \package ->
    case package ^. parentPackageId of
      Just parentPackageId ->
        heGetAllPreviousEventsSincePackageId parentPackageId $ \pkgEvents ->
          return . Right $ pkgEvents ++ (package ^. events)
      Nothing -> return . Right $ package ^. events

getAllPreviousEventsSincePackageIdAndUntilPackageId :: String -> String -> AppContextM (Either AppError [Event])
getAllPreviousEventsSincePackageIdAndUntilPackageId sincePkgId untilPkgId = go sincePkgId
  where
    go pkgId =
      if pkgId == untilPkgId
        then return . Right $ []
        else heFindPackageWithEventsById pkgId $ \package ->
               case package ^. parentPackageId of
                 Just parentPackageId -> do
                   eitherPkgEvents <- go parentPackageId
                   case eitherPkgEvents of
                     Right pkgEvents -> return . Right $ pkgEvents ++ (package ^. events)
                     Left error -> return . Left $ error
                 Nothing -> return . Right $ package ^. events

getEventsForBranchUuid :: String -> AppContextM (Either AppError [Event])
getEventsForBranchUuid branchUuid =
  heFindBranchWithEventsById branchUuid $ \branch ->
    case branch ^. parentPackageId of
      Just ppId ->
        heGetAllPreviousEventsSincePackageId ppId $ \eventsFromPackage -> do
          let eventsFromKM = branch ^. events
          let pkgEvents = eventsFromPackage ++ eventsFromKM
          return . Right $ pkgEvents
      Nothing -> return . Right $ branch ^. events

getNewerPackages :: String -> AppContextM (Either AppError [Package])
getNewerPackages currentPkgId =
  heFindPackagesByOrganizationIdAndKmId pkgOrganizationId pkgKmId $ \packages -> do
    let packagesWithHigherVersion =
          filter (\pkg -> isNothing $ validateIsVersionHigher (pkg ^. version) pkgVersion) packages
    return . Right . sortPackagesByVersion $ packagesWithHigherVersion
  where
    pkgOrganizationId = T.unpack $ splitPackageId currentPkgId !! 0
    pkgKmId = T.unpack $ splitPackageId currentPkgId !! 1
    pkgVersion = T.unpack $ splitPackageId currentPkgId !! 2

createPackage :: String -> String -> String -> String -> String -> Maybe String -> [Event] -> AppContextM PackageDTO
createPackage name organizationId kmId version description maybeParentPackageId events = do
  let package = buildPackage name organizationId kmId version description maybeParentPackageId events
  insertPackage package
  return $ packageWithEventsToDTO package

createPackageFromKMC :: String -> String -> VersionDTO -> AppContextM (Either AppError PackageDTO)
createPackageFromKMC branchUuid pkgVersion versionDto =
  heValidateVersionFormat pkgVersion $
  heFindBranchWithEventsById branchUuid $ \branch ->
    heGetOrganization $ \organization ->
      validateVersion pkgVersion branch organization $
      getEventsForPackage branch $ \events -> do
        let pkgName = branch ^. name
        let pkgOrganizationId = organization ^. organizationId
        let pkgKmId = branch ^. kmId
        let mPpId = branch ^. parentPackageId
        let pkgDescription = versionDto ^. description
        createdPackage <- createPackage pkgName pkgOrganizationId pkgKmId pkgVersion pkgDescription mPpId events
        deleteEventsAtBranch branchUuid
        updateBranchWithParentPackageId branchUuid (createdPackage ^. pId)
        updateBranchIfMigrationIsCompleted branchUuid
        deleteMigratorStateByBranchUuid branchUuid
        recompileKnowledgeModel branch $ return . Right $ createdPackage
  where
    validateVersion pkgVersion branch organization callback = do
      let pkgOrganizationId = organization ^. organizationId
      let pkgKmId = branch ^. kmId
      eitherMaybePackage <- getTheNewestPackageByOrganizationIdAndKmId pkgOrganizationId pkgKmId
      case eitherMaybePackage of
        Right (Just package) ->
          case validateIsVersionHigher pkgVersion (package ^. version) of
            Nothing -> callback
            Just error -> return . Left $ error
        Right Nothing -> callback
        Left error -> return . Left $ error
    updateBranchIfMigrationIsCompleted branchUuid = do
      eitherMigrationState <- findMigratorStateByBranchUuid branchUuid
      case eitherMigrationState of
        Right migrationState -> do
          let msBranchParentId = migrationState ^. branchParentId
          let msTargetPackageId = migrationState ^. targetPackageId
          updateBranchWithMigrationInfo branchUuid msTargetPackageId msBranchParentId
        Left _ -> return ()
    getEventsForPackage branch callback = do
      let branchUuid = U.toString $ branch ^. uuid
      eitherMigrationState <- findMigratorStateByBranchUuid branchUuid
      case eitherMigrationState of
        Right migrationState -> callback $ migrationState ^. resultEvents
        Left (NotExistsError _) -> callback $ branch ^. events
        Left error -> return . Left $ error
    recompileKnowledgeModel branch callback = do
      let branchUuid = U.toString $ branch ^. uuid
      heGetEventsForBranchUuid branchUuid $ \eventsForBranchUuid -> do
        recompileKnowledgeModelWithEvents branchUuid eventsForBranchUuid
        callback

deletePackagesByQueryParams :: [(Text, Text)] -> AppContextM (Maybe AppError)
deletePackagesByQueryParams queryParams =
  hmFindPackagesFiltered queryParams $ \packages -> do
    maybeError <- validatePackagesDeletation (_packagePId <$> packages)
    if isJust maybeError
      then return maybeError
      else do
        deletePackagesFiltered queryParams
        return Nothing

deletePackage :: String -> AppContextM (Maybe AppError)
deletePackage pkgId =
  hmFindPackageById pkgId $ \package -> do
    maybeError <- validatePackageDeletation pkgId
    if isJust maybeError
      then return maybeError
      else do
        deletePackageById pkgId
        return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetSeriesOfPackages pkgId callback = do
  eitherPackages <- getSeriesOfPackages pkgId
  case eitherPackages of
    Right package -> callback package
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetAllPreviousEventsSincePackageId pkgId callback = do
  eitherEvents <- getAllPreviousEventsSincePackageId pkgId
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Left $ error

hmGetAllPreviousEventsSincePackageId pkgId callback = do
  eitherEvents <- getAllPreviousEventsSincePackageId pkgId
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Just $ error

-- -----------------------------------------------------
heGetAllPreviousEventsSincePackageIdAndUntilPackageId since until callback = do
  eitherEvents <- getAllPreviousEventsSincePackageIdAndUntilPackageId since until
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetEventsForBranchUuid branchUuid callback = do
  eitherEvents <- getEventsForBranchUuid branchUuid
  case eitherEvents of
    Right events -> callback events
    Left error -> return . Left $ error

-- -----------------------------------------------------
heGetNewerPackages currentPkgId callback = do
  eitherPackages <- getNewerPackages currentPkgId
  case eitherPackages of
    Right packages -> callback packages
    Left error -> return . Left $ error
