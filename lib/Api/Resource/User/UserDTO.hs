module Api.Resource.User.UserDTO where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson
import Data.UUID

import Common.Types

data UserDTO = UserDTO
  { _udtoUuid :: UUID
  , _udtoName :: String
  , _udtoSurname :: String
  , _udtoEmail :: Email
  , _udtoRole :: Role
  , _udtoPermissions :: [Permission]
  , _udtoIsActive :: Bool
  } deriving (Show, Eq)

makeLenses ''UserDTO

instance FromJSON UserDTO where
  parseJSON (Object o) = do
    _udtoUuid <- o .: "uuid"
    _udtoName <- o .: "name"
    _udtoSurname <- o .: "surname"
    _udtoEmail <- o .: "email"
    _udtoRole <- o .: "role"
    _udtoPermissions <- o .: "permissions"
    _udtoIsActive <- o .: "isActive"
    return UserDTO {..}
  parseJSON _ = mzero

instance ToJSON UserDTO where
  toJSON UserDTO {..} =
    object
      [ "uuid" .= _udtoUuid
      , "name" .= _udtoName
      , "surname" .= _udtoSurname
      , "email" .= _udtoEmail
      , "role" .= _udtoRole
      , "permissions" .= _udtoPermissions
      , "isActive" .= _udtoIsActive
      ]
