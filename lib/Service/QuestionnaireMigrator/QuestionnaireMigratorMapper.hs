module Service.QuestionnaireMigrator.QuestionnaireMigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Model.Package.Package
import Model.KnowledgeModel.KnowledgeModel
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Service.Package.PackageMapper
import Service.Event.EventMapper
import qualified Service.Questionnaire.QuestionnaireMapper as QM
import qualified Service.KnowledgeModel.KnowledgeModelMapper as KM

toDTO :: QuestionnaireMigratorState -> Package -> KnowledgeModel -> QuestionnaireMigratorStateDTO
toDTO model pkg km = QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire = qtnDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel = KM.toKnowledgeModelDTO $ model ^. diffKnowledgeModel
  , _questionnaireMigratorStateDTOTargetPackageId = model ^. targetPackageId
  , _questionnaireMigratorStateDTODiffEvents = toDTOs $ model ^. diffEvents
  }
  where qtnDTO = QM.toDetailWithPackageDTO (model ^. questionnaire) pkgDTO km
        pkgDTO = packageToDTO pkg
