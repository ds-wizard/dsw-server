module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO

instance FromJSON QuestionnaireMigratorStateDTO where
  parseJSON (Object o) = do
    _questionnaireMigratorStateDTOQuestionnaire <- o .: "questionnaire"
    _questionnaireMigratorStateDTODiffKnowledgeModel <- o .: "diffKnowledgeModel"
    _questionnaireMigratorStateDTOPreviousKnowledgeModel <- o .: "previousKnowledgeModel"
    _questionnaireMigratorStateDTODiffEvents <- o .: "diffEvents"
    _questionnaireMigratorStateDTOTargetPackageId <- o .: "targetPackageId"
    return QuestionnaireMigratorStateDTO {..}
  parseJSON _ = mzero

instance ToJSON QuestionnaireMigratorStateDTO where
  toJSON QuestionnaireMigratorStateDTO {..} =
    object
      [ "questionnaire" .= _questionnaireMigratorStateDTOQuestionnaire
      , "diffKnowledgeModel" .= _questionnaireMigratorStateDTODiffKnowledgeModel
      , "previousKnowledgeModel" .= _questionnaireMigratorStateDTOPreviousKnowledgeModel
      , "diffEvents" .= _questionnaireMigratorStateDTODiffEvents
      , "targetPackageId" .= _questionnaireMigratorStateDTOTargetPackageId
      ]
