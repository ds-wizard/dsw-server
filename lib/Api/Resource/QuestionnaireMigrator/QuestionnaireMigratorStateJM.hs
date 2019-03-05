module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO

instance FromJSON QuestionnaireMigratorStateDTO where
  parseJSON (Object o) = do
    _questionnaireMigratorStateDTOQuestionnaire <- o .: "questionnaire"
    _questionnaireMigratorStateDTODiffKnowledgeModel <- o .: "diffKnowledgeModel"
    _questionnaireMigratorStateDTODiffEvents <- o .: "diffEvents"
    _questionnaireMigratorStateDTOTargetPackageId <- o.: "targetPackageId"
    return QuestionnaireMigratorStateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireMigratorStateDTO where
  toJSON QuestionnaireMigratorStateDTO {..} =
    object
      [ "questionnaire" .= _questionnaireMigratorStateDTOQuestionnaire
      , "diffKnowledgeModel" .= _questionnaireMigratorStateDTODiffKnowledgeModel
      , "diffEvents" .= _questionnaireMigratorStateDTODiffEvents
      , "targetPackageId" .= _questionnaireMigratorStateDTOTargetPackageId
      ]
