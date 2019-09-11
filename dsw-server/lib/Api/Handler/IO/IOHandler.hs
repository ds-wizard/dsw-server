module Api.Handler.IO.IOHandler where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as LT
import Network.Wai.Parse
import Web.Scotty.Trans (addHeader, files, json, param, raw)

import Api.Handler.Common
import Api.Resource.Package.PackageSimpleJM ()
import Api.Resource.PackageBundle.PackageBundleJM ()
import Localization.Messages.Public
import Model.Error.Error
import Service.PackageBundle.PackageBundleService
import Util.String (stripSuffix)

exportA :: Endpoint
exportA = do
  pId <- param "pId"
  eitherDto <- runInUnauthService $ exportPackageBundle pId
  case eitherDto of
    Right dto -> do
      let cdHeader = "attachment;filename=" ++ pId ++ ".km"
      addHeader "Content-Disposition" (LT.pack cdHeader)
      addHeader "Content-Type" (LT.pack "application/octet-stream")
      raw $ encode dto
    Left error -> sendError error

importA :: Endpoint
importA =
  getAuthServiceExecutor $ \runInAuthService -> do
    fs <- files
    if length fs > 0
      then do
        let (_, file) = fs !! 0
        let fName = BS.unpack . fileName $ file
        let fContent = fileContent file
        eitherDto <-
          case stripSuffix ".ttl" fName of
            Just name -> runInAuthService $ importShaclFromFile name fContent
            Nothing -> runInAuthService $ importPackageBundleFromFile fContent
        case eitherDto of
          Right dto -> json dto
          Left error -> sendError error
      else sendError (UserError _ERROR_VALIDATION__FILE_ABSENCE)
