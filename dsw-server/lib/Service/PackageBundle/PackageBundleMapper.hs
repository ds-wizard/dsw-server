module Service.PackageBundle.PackageBundleMapper where

import Control.Lens ((^.))
import Data.Time

import Api.Resource.Event.EventDTO
import Api.Resource.Package.PackageDTO
import Api.Resource.PackageBundle.PackageBundleDTO
import Constant.KnowledgeModel
import LensesConfig
import Model.PackageBundle.PackageBundle
import qualified Service.Package.PackageMapper as PM
import Service.Package.PackageUtils

toDTO :: PackageBundle -> PackageBundleDTO
toDTO pb =
  PackageBundleDTO
  { _packageBundleDTOBundleId = pb ^. bundleId
  , _packageBundleDTOName = pb ^. name
  , _packageBundleDTOOrganizationId = pb ^. organizationId
  , _packageBundleDTOKmId = pb ^. kmId
  , _packageBundleDTOVersion = pb ^. version
  , _packageBundleDTOMetamodelVersion = pb ^. metamodelVersion
  , _packageBundleDTOPackages = PM.toDTO <$> pb ^. packages
  }

fromShacl :: String -> [EventDTO] -> UTCTime -> PackageBundleDTO
fromShacl pId events now =
  PackageBundleDTO
  { _packageBundleDTOBundleId = pId
  , _packageBundleDTOName = pId
  , _packageBundleDTOOrganizationId = getOrgIdFromPkgId pId
  , _packageBundleDTOKmId = getKmIdFromPkgId pId
  , _packageBundleDTOVersion = getVersionFromPkgId pId
  , _packageBundleDTOMetamodelVersion = kmMetamodelVersion
  , _packageBundleDTOPackages =
      [ PackageDTO
        { _packageDTOPId = pId
        , _packageDTOName = pId
        , _packageDTOOrganizationId = getOrgIdFromPkgId pId
        , _packageDTOKmId = getKmIdFromPkgId pId
        , _packageDTOVersion = getVersionFromPkgId pId
        , _packageDTOMetamodelVersion = kmMetamodelVersion
        , _packageDTODescription = ""
        , _packageDTOReadme = ""
        , _packageDTOLicense = ""
        , _packageDTOPreviousPackageId = Nothing
        , _packageDTOForkOfPackageId = Nothing
        , _packageDTOMergeCheckpointPackageId = Nothing
        , _packageDTOEvents = events
        , _packageDTOCreatedAt = now
        }
      ]
  }
