module Api.Handler.Shacl.ShaclHandler where

import Web.Scotty.Trans (body, json)

import Api.Handler.Common
import Api.Resource.KnowledgeModel.KnowledgeModelChangeJM ()
import Api.Resource.KnowledgeModel.KnowledgeModelDTO ()
import Service.Shacl.ShaclService

postShaclsPreviewA :: Endpoint
postShaclsPreviewA =
  checkPermission "QTN_PERM" $
  getAuthServiceExecutor $ \runInAuthService -> do
    reqBody <- body
    eitherResDto <- runInAuthService $ createShaclPreview reqBody
    case eitherResDto of
      Right resDto -> json resDto
      Left error -> sendError error
