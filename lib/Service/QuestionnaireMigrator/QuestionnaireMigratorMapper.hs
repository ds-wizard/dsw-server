module Service.QuestionnaireMigrator.QuestionnaireMigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Model.Package.Package
import Model.KnowledgeModel.KnowledgeModel
import Model.Questionnaire.QuestionnaireState
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Service.Package.PackageMapper
import Service.Event.EventMapper
import qualified Service.Questionnaire.QuestionnaireMapper as QM
import qualified Service.KnowledgeModel.KnowledgeModelMapper as KM

toDTO :: QuestionnaireMigratorState -> Package -> KnowledgeModel -> QuestionnaireState -> QuestionnaireMigratorStateDTO
toDTO model pkg km state = QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire = qtnDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel = KM.toKnowledgeModelDTO $ model ^. diffKnowledgeModel
  , _questionnaireMigratorStateDTOTargetPackageId = model ^. targetPackageId
  , _questionnaireMigratorStateDTODiffEvents = toDTOs $ model ^. diffEvents
  }
  where qtnDTO = QM.toDetailWithPackageDTO (model ^. questionnaire) pkgDTO km state
        pkgDTO = packageToDTO pkg
