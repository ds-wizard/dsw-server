module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO where

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Event.EventDTO

data QuestionnaireMigratorStateDTO = QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire :: QuestionnaireDetailDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireMigratorStateDTOPreviousKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireMigratorStateDTODiffEvents :: [EventDTO]
  , _questionnaireMigratorStateDTOTargetPackageId :: String
  }
