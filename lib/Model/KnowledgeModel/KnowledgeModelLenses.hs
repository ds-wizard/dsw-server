module Model.KnowledgeModel.KnowledgeModelLenses where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

-- -------------------------
-- KNOWLEDGE MODEL ---------
-- -------------------------
chaptersL :: Functor f => ([Chapter] -> f [Chapter]) -> KnowledgeModel -> f KnowledgeModel
chaptersL = createEntityLFn (entities . chapters)

chaptersM :: Functor f => ((M.Map U.UUID Chapter) -> f (M.Map U.UUID Chapter)) -> KnowledgeModel -> f KnowledgeModel
chaptersM = createEntityMFn (entities . chapters)

------------------------------------------------------------------------------------------
questionsL :: Functor f => ([Question] -> f [Question]) -> KnowledgeModel -> f KnowledgeModel
questionsL = createEntityLFn (entities . questions)

questionsM :: Functor f => ((M.Map U.UUID Question) -> f (M.Map U.UUID Question)) -> KnowledgeModel -> f KnowledgeModel
questionsM = createEntityMFn (entities . questions)

------------------------------------------------------------------------------------------
answersL :: Functor f => ([Answer] -> f [Answer]) -> KnowledgeModel -> f KnowledgeModel
answersL = createEntityLFn (entities . answers)

answersM :: Functor f => ((M.Map U.UUID Answer) -> f (M.Map U.UUID Answer)) -> KnowledgeModel -> f KnowledgeModel
answersM = createEntityMFn (entities . answers)

------------------------------------------------------------------------------------------
expertsL :: Functor f => ([Expert] -> f [Expert]) -> KnowledgeModel -> f KnowledgeModel
expertsL = createEntityLFn (entities . experts)

expertsM :: Functor f => ((M.Map U.UUID Expert) -> f (M.Map U.UUID Expert)) -> KnowledgeModel -> f KnowledgeModel
expertsM = createEntityMFn (entities . experts)

------------------------------------------------------------------------------------------
referencesL :: Functor f => ([Reference] -> f [Reference]) -> KnowledgeModel -> f KnowledgeModel
referencesL = createEntityLFn (entities . references)

referencesM :: Functor f => ((M.Map U.UUID Reference) -> f (M.Map U.UUID Reference)) -> KnowledgeModel -> f KnowledgeModel
referencesM = createEntityMFn (entities . references)

------------------------------------------------------------------------------------------
integrationsL :: Functor f => ([Integration] -> f [Integration]) -> KnowledgeModel -> f KnowledgeModel
integrationsL = createEntityLFn (entities . integrations)

integrationsM :: Functor f => ((M.Map U.UUID Integration) -> f (M.Map U.UUID Integration)) -> KnowledgeModel -> f KnowledgeModel
integrationsM = createEntityMFn (entities . integrations)

------------------------------------------------------------------------------------------
tagsL :: Functor f => ([Tag] -> f [Tag]) -> KnowledgeModel -> f KnowledgeModel
tagsL = createEntityLFn (entities . tags)

tagsM :: Functor f => ((M.Map U.UUID Tag) -> f (M.Map U.UUID Tag)) -> KnowledgeModel -> f KnowledgeModel
tagsM = createEntityMFn (entities . tags)

------------------------------------------------------------------------------------------
createEntityLFn :: (EntityAccesors a, Functor f) => (Lens' KnowledgeModel (M.Map U.UUID a)) -> ([a] -> f [a]) -> KnowledgeModel -> f KnowledgeModel
createEntityLFn accessor convert km = fmap (update km) (convert . M.elems $ km ^. accessor)
  where
    update km newValue = km & accessor .~ (toMap newValue)

createEntityMFn :: Functor f => (Lens' KnowledgeModel (M.Map U.UUID a)) -> (M.Map U.UUID a -> f (M.Map U.UUID a)) -> KnowledgeModel -> f KnowledgeModel
createEntityMFn accessor convert km = fmap (update km) (convert $ km ^. accessor)
  where
    update km newValue = km & accessor .~ newValue

toMap :: EntityAccesors a => [a] -> M.Map U.UUID a
toMap = M.fromList . fmap (\entity -> (getEntityUuid entity, entity))

------------------------------------------------------------------------------------------
tagUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
tagUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (ListQuestion' q) = q ^. tagUuids
    get (OptionsQuestion' q) = q ^. tagUuids
    get (ValueQuestion' q) = q ^. tagUuids
    get (IntegrationQuestion' q) = q ^. tagUuids
    set :: Question -> [U.UUID] -> Question
    set (ListQuestion' q) newValue = ListQuestion' $ q & tagUuids .~ newValue
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & tagUuids .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & tagUuids .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & tagUuids .~ newValue

-- ------------------------------------------------------------------------------------------
expertUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
expertUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (ListQuestion' q) = q ^. expertUuids
    get (OptionsQuestion' q) = q ^. expertUuids
    get (ValueQuestion' q) = q ^. expertUuids
    get (IntegrationQuestion' q) = q ^. expertUuids
    set :: Question -> [U.UUID] -> Question
    set (ListQuestion' q) newValue = ListQuestion' $ q & expertUuids .~ newValue
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & expertUuids .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & expertUuids .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & expertUuids .~ newValue

-- ------------------------------------------------------------------------------------------
referenceUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
referenceUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (ListQuestion' q) = q ^. referenceUuids
    get (OptionsQuestion' q) = q ^. referenceUuids
    get (ValueQuestion' q) = q ^. referenceUuids
    get (IntegrationQuestion' q) = q ^. referenceUuids
    set :: Question -> [U.UUID] -> Question
    set (ListQuestion' q) newValue = ListQuestion' $ q & referenceUuids .~ newValue
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & referenceUuids .~ newValue
    set (ValueQuestion' q) newValue = ValueQuestion' $ q & referenceUuids .~ newValue
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & referenceUuids .~ newValue

-- ------------------------------------------------------------------------------------------
answerUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
answerUuids'  convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (OptionsQuestion' q) = q ^. answerUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set (OptionsQuestion' q) newValue = OptionsQuestion' $ q & answerUuids .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
integrationUuid' :: Functor f => (U.UUID -> f U.UUID) -> Question -> f Question
integrationUuid' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> U.UUID
    get (IntegrationQuestion' q) = q ^. integrationUuid
    get q = U.nil
    set :: Question -> U.UUID -> Question
    set (IntegrationQuestion' q) newValue = IntegrationQuestion' $ q & integrationUuid .~ newValue
    set q newValue = q

-- ------------------------------------------------------------------------------------------
itemTemplateQuestionUuids' :: Functor f => ([U.UUID] -> f [U.UUID]) -> Question -> f Question
itemTemplateQuestionUuids' convert entity = fmap (set entity) (convert . get $ entity)
  where
    get :: Question -> [U.UUID]
    get (ListQuestion' q) = q ^. itemTemplateQuestionUuids
    get q = []
    set :: Question -> [U.UUID] -> Question
    set (ListQuestion' q) newValue = ListQuestion' $ q & itemTemplateQuestionUuids .~ newValue
    set q newValue = q
