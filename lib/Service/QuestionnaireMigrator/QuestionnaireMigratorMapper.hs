module Service.QuestionnaireMigrator.QuestionnaireMigratorMapper where

import Control.Lens ((^.))

import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelDiff.KnowledgeModelDiff
import Model.Package.Package
import Model.Questionnaire.QuestionnaireState
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Service.Event.EventMapper
import qualified Service.KnowledgeModel.KnowledgeModelMapper as KM
import qualified Service.Package.PackageMapper as PM
import qualified Service.Questionnaire.QuestionnaireMapper as QM

toDTO ::
     QuestionnaireMigratorState
  -> KnowledgeModelDiff
  -> Package
  -> KnowledgeModel
  -> QuestionnaireState
  -> QuestionnaireMigratorStateDTO
toDTO model diff pkg km qtnState =
  QuestionnaireMigratorStateDTO
  { _questionnaireMigratorStateDTOQuestionnaire = qtnDTO
  , _questionnaireMigratorStateDTODiffKnowledgeModel = KM.toKnowledgeModelDTO $ diff ^. diffKnowledgeModel
  , _questionnaireMigratorStateDTOPreviousKnowledgeModel = KM.toKnowledgeModelDTO $ diff ^. previousKnowledgeModel
  , _questionnaireMigratorStateDTOTargetPackageId = model ^. targetPackageId
  , _questionnaireMigratorStateDTODiffEvents = toDTOs $ diff ^. diffEvents
  }
  where
    qtnDTO = QM.toDetailWithPackageDTO (model ^. questionnaire) (PM.packageToDTO pkg) km qtnState
