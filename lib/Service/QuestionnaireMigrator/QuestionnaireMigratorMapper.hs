module Service.QuestionnaireMigrator.QuestionnaireMigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Service.Questionnaire.QuestionnaireMapper

--fromDTO :: QuestionnaireMigratorStateDTO -> QuestionnaireMigratorState
--fromDTO dto = QuestionnaireMigratorState
--  { _questionnaireMigratorStateQuestionnaire = dto ^. questionnaire
--  , _questionnaireMigratorStateDiffKnowledgeModel = dto ^. diffKnowledgeModel
--  , _questionnaireMigratorStateTargetPackageId = dto ^. targetPackageId
--  }
--
--toDTO :: QuestionnaireMigratorState -> QuestionnaireMigratorStateDTO
--toDTO model = QuestionnaireMigratorStateDTO
--  { _questionnaireMigratorStateDTOQuestionnaire = model ^. questionnaire
--  , _questionnaireMigratorStateDTODiffKnowledgeModel = model ^. diffKnowledgeModel
--  , _questionnaireMigratorStateDTOTargetPackageId = model ^. targetPackageId
--  }
