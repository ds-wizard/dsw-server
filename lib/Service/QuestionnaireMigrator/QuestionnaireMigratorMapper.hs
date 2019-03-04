module Service.QuestionnaireMigrator.QuestionnaireMigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Model.Package.Package
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Service.Package.PackageMapper
import qualified Service.Questionnaire.QuestionnaireMapper as QM
import qualified Service.KnowledgeModel.KnowledgeModelMapper as KM

toDTO :: QuestionnaireMigratorState -> Package -> QuestionnaireMigratorStateDTO
toDTO model pkg = QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire = qtnDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel = KM.toKnowledgeModelDTO $ model ^. diffKnowledgeModel
  , _questionnaireMigratorStateDTOTargetPackageId = model ^. targetPackageId
  }
  where qtnDTO = QM.toDetailWithPackageDTO (model ^. questionnaire) pkgDTO
        pkgDTO = packageToDTO pkg
