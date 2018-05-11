module Api.Handler.ActionKey.ActionKeyHandler where

import Control.Monad.Reader (asks, liftIO)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (created201)
import Web.Scotty.Trans (status)

import Api.Handler.Common
import Model.Context.AppContext
import Service.User.UserService

postActionKeysA :: Endpoint
postActionKeysA = do
  dswConfig <- lift . asks $ _appContextConfig
  context <- lift . asks $ _appContextOldContext
  getReqDto $ \reqDto -> do
    maybeError <- liftIO $ resetUserPassword context dswConfig reqDto
    case maybeError of
      Nothing -> status created201
      Just error -> sendError error
