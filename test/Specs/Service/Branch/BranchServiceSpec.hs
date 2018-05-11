module Specs.Service.Branch.BranchServiceSpec where

import Control.Lens
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Api.Resource.Migrator.MigratorConflictDTO
import Api.Resource.Migrator.MigratorStateCreateDTO
import Database.DAO.Event.EventDAO
import Database.DAO.Package.PackageDAO
import qualified Database.Migration.Branch.BranchMigration as B
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Package.Data.Package
import qualified Database.Migration.Package.PackageMigration as PKG
import LensesConfig
import Model.Branch.BranchState
import Model.Migrator.MigratorState
import Model.Package.Package
import Service.Branch.BranchService
import Service.Migrator.MigratorService

branchServiceSpec =
  describe "Package Service" $
  it "isVersionInValidFormat" $ do
    isNothing (isValidArtifactId "core") `shouldBe` True
    isNothing (isValidArtifactId "ab") `shouldBe` True
    isNothing (isValidArtifactId "core-nl") `shouldBe` True
    isNothing (isValidArtifactId "core-nl-amsterdam") `shouldBe` True
    isJust (isValidArtifactId "a") `shouldBe` True
    isJust (isValidArtifactId "core.nl") `shouldBe` True
    isJust (isValidArtifactId "a.b") `shouldBe` True
    isJust (isValidArtifactId "core_nl") `shouldBe` True

branchServiceIntegrationSpec appContext = do
  let context = appContext ^. oldContext
  let dswConfig = appContext ^. config
  describe "Branch Service Integration" $ do
    let branchUuid = "6474b24b-262b-42b1-9451-008e8363f2b6"
    describe "getBranchState" $ do
      it "BSDefault - no edit events, no new parent package version" $
        -- GIVEN: Prepare database
       do
        liftIO . runNoLoggingT $ PKG.runMigration appContext
        liftIO . runNoLoggingT $ B.runMigration appContext
        liftIO $ deleteEventsAtBranch context branchUuid
        liftIO $ deletePackageById context (elixirNlPackage2Dto ^. pkgweId)
        -- AND: Prepare expectations
        let expState = BSDefault
        -- WHEN:
        eitherResState <- liftIO $ getBranchState context branchUuid
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSEdited - edit events" $
        -- GIVEN: Prepare database
       do
        liftIO . runNoLoggingT $ PKG.runMigration appContext
        liftIO . runNoLoggingT $ B.runMigration appContext
        -- AND: Prepare expectations
        let expState = BSEdited
        -- WHEN:
        eitherResState <- liftIO $ getBranchState context branchUuid
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSEdited - edit events and new parent package version is available" $
        -- GIVEN: Prepare database
       do
        liftIO . runNoLoggingT $ PKG.runMigration appContext
        liftIO . runNoLoggingT $ B.runMigration appContext
        liftIO $ insertPackage context elixirNlPackage2Dto
        -- AND: Prepare expectations
        let expState = BSEdited
        -- WHEN:
        eitherResState <- liftIO $ getBranchState context branchUuid
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSOutdated - no edit events and new parent package version is available" $
        -- GIVEN: Prepare database
       do
        liftIO . runNoLoggingT $ PKG.runMigration appContext
        liftIO . runNoLoggingT $ B.runMigration appContext
        liftIO $ insertPackage context elixirNlPackage2Dto
        liftIO $ deleteEventsAtBranch context branchUuid
        -- AND: Prepare expectations
        let expState = BSOutdated
        -- WHEN:
        eitherResState <- liftIO $ getBranchState context branchUuid
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSMigrating - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
       do
        liftIO . runNoLoggingT $ PKG.runMigration appContext
        liftIO . runNoLoggingT $ B.runMigration appContext
        liftIO $ insertPackage context elixirNlPackage2Dto
        liftIO $ deleteEventsAtBranch context branchUuid
        let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
        liftIO $ createMigration context branchUuid migratorCreateDto
        -- AND: Prepare expectations
        let expState = BSMigrating
        -- WHEN:
        eitherResState <- liftIO $ getBranchState context branchUuid
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
      it "BSMigrated - no edit events and new parent package version is available and migration is in process" $
        -- GIVEN: Prepare database
       do
        liftIO . runNoLoggingT $ PKG.runMigration appContext
        liftIO . runNoLoggingT $ B.runMigration appContext
        liftIO $ deleteEventsAtBranch context branchUuid
        let migratorCreateDto = MigratorStateCreateDTO {_mscdtoTargetPackageId = elixirNlPackage2Dto ^. pkgweId}
        liftIO $ createMigration context branchUuid migratorCreateDto
        let reqDto =
              MigratorConflictDTO
              {_mcdtoOriginalEventUuid = a_km1_ch3 ^. uuid, _mcdtoAction = MCAReject, _mcdtoEvent = Nothing}
        liftIO $ solveConflictAndMigrate context branchUuid reqDto
        -- AND: Prepare expectations
        let expState = BSMigrated
        -- WHEN:
        eitherResState <- liftIO $ getBranchState context branchUuid
        -- THEN:
        liftIO $ isRight eitherResState `shouldBe` True
        let (Right resState) = eitherResState
        resState `shouldBe` expState
