module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO where

import qualified Data.UUID as U

import Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO

data QuestionnaireMigratorStateDTO = QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire :: QuestionnaireDetailDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireMigratorStateDTOTargetPackageId :: U.UUID
  }
