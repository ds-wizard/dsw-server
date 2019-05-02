module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO

instance FromJSON QuestionnaireMigratorStateCreateDTO where
  parseJSON (Object o) = do
    _questionnaireMigratorStateCreateDTOTargetPackageId <- o .: "targetPackageId"
    return QuestionnaireMigratorStateCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireMigratorStateCreateDTO where
  toJSON QuestionnaireMigratorStateCreateDTO {..} =
    object ["targetPackageId" .= _questionnaireMigratorStateCreateDTOTargetPackageId]
