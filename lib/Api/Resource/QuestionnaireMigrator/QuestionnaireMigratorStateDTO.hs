module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO where

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO

data QuestionnaireMigratorStateDTO = QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire :: QuestionnaireDetailDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireMigratorStateDTOTargetPackageId :: String
  }
