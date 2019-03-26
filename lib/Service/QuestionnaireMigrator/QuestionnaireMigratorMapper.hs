module Service.QuestionnaireMigrator.QuestionnaireMigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Model.Package.Package
import Model.KnowledgeModel.KnowledgeModel
import Model.Questionnaire.QuestionnaireState
import Model.QuestionnaireMigrator.QuestionnaireMigratorState
import Model.KnowledgeModelDiff.KnowledgeModelDiff
import Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateDTO
import Service.Event.EventMapper
import qualified Service.Questionnaire.QuestionnaireMapper as QM
import qualified Service.KnowledgeModel.KnowledgeModelMapper as KM

toDTO :: QuestionnaireMigratorState -> KnowledgeModelDiff -> Package -> KnowledgeModel -> QuestionnaireState -> QuestionnaireMigratorStateDTO
toDTO model diff pkg km qtnState =
  QuestionnaireMigratorStateDTO
    { _questionnaireMigratorStateDTOQuestionnaire = qtnDTO
    , _questionnaireMigratorStateDTODiffKnowledgeModel = KM.toKnowledgeModelDTO $ diff ^. diffKnowledgeModel
    , _questionnaireMigratorStateDTOPreviousKnowledgeModel = KM.toKnowledgeModelDTO $ diff ^. previousKnowledgeModel
    , _questionnaireMigratorStateDTOTargetPackageId = model ^. targetPackageId
    , _questionnaireMigratorStateDTODiffEvents = toDTOs $ diff ^. diffEvents
    }
  where qtnDTO = QM.toDTO (model ^. questionnaire) pkg qtnState
