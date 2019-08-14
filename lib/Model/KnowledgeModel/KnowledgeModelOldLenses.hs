module Model.KnowledgeModel.KnowledgeModelOldLenses where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
qChangeTitle :: (String -> Identity String) -> Question -> Identity Question
qChangeTitle convert q = Identity . set $ q
  where
    newValue :: String
    newValue = runIdentity . convert $ ""
    set :: Question -> Question
    set (OptionsQuestion' q) = OptionsQuestion' $ q & title .~ newValue
    set (ListQuestion' q) = ListQuestion' $ q & title .~ newValue
    set (ValueQuestion' q) = ValueQuestion' $ q & title .~ newValue
    set (IntegrationQuestion' q) = IntegrationQuestion' $ q & title .~ newValue

------------------------------------------------------------------------------------------
qChangeText :: (Maybe String -> Identity (Maybe String)) -> Question -> Identity Question
qChangeText convert q = Identity . set $ q
  where
    newValue :: Maybe String
    newValue = runIdentity . convert $ Nothing
    set :: Question -> Question
    set (OptionsQuestion' q) = OptionsQuestion' $ q & text .~ newValue
    set (ListQuestion' q) = ListQuestion' $ q & text .~ newValue
    set (ValueQuestion' q) = ValueQuestion' $ q & text .~ newValue
    set (IntegrationQuestion' q) = IntegrationQuestion' $ q & text .~ newValue

------------------------------------------------------------------------------------------
qChangeRequiredLevel :: (Maybe Int -> Identity (Maybe Int)) -> Question -> Identity Question
qChangeRequiredLevel convert q = Identity . set $ q
  where
    newValue :: Maybe Int
    newValue = runIdentity . convert $ Nothing
    set :: Question -> Question
    set (OptionsQuestion' q) = OptionsQuestion' $ q & requiredLevel .~ newValue
    set (ListQuestion' q) = ListQuestion' $ q & requiredLevel .~ newValue
    set (ValueQuestion' q) = ValueQuestion' $ q & requiredLevel .~ newValue
    set (IntegrationQuestion' q) = IntegrationQuestion' $ q & requiredLevel .~ newValue

------------------------------------------------------------------------------------------
getTagUuids :: Question -> [U.UUID]
getTagUuids (OptionsQuestion' q) = q ^. tagUuids
getTagUuids (ListQuestion' q) = q ^. tagUuids
getTagUuids (ValueQuestion' q) = q ^. tagUuids
getTagUuids (IntegrationQuestion' q) = q ^. tagUuids

qChangeTagUuids :: ([U.UUID] -> Identity [U.UUID]) -> Question -> Identity Question
qChangeTagUuids convert q = Identity . set $ q
  where
    newValue :: [U.UUID]
    newValue = runIdentity . convert $ []
    set :: Question -> Question
    set (OptionsQuestion' q) = OptionsQuestion' $ q & tagUuids .~ newValue
    set (ListQuestion' q) = ListQuestion' $ q & tagUuids .~ newValue
    set (ValueQuestion' q) = ValueQuestion' $ q & tagUuids .~ newValue
    set (IntegrationQuestion' q) = IntegrationQuestion' $ q & tagUuids .~ newValue

-- ------------------------------------------------------------------------------------------
qChangeItemTemplateTitle :: (String -> Identity String) -> Question -> Identity Question
qChangeItemTemplateTitle convert q = Identity . set $ q
  where
    newValue :: String
    newValue = runIdentity . convert $ ""
    set :: Question -> Question
    set (ListQuestion' q) = ListQuestion' $ q & itemTemplateTitle .~ newValue
    set q = q

qChangeItemTemplateQuestionUuids :: ([U.UUID] -> Identity [U.UUID]) -> Question -> Identity Question
qChangeItemTemplateQuestionUuids convert q = Identity . set $ q
  where
    newValue :: [U.UUID]
    newValue = runIdentity . convert $ []
    set :: Question -> Question
    set (ListQuestion' q) = ListQuestion' $ q & itemTemplateQuestionUuids .~ newValue
    set q = q

-- ------------------------------------------------------------------------------------------
qChangeValueType :: (QuestionValueType -> Identity QuestionValueType) -> Question -> Identity Question
qChangeValueType convert = Identity . set
  where
    newValue :: QuestionValueType
    newValue = runIdentity . convert $ StringQuestionValueType
    set :: Question -> Question
    set (ValueQuestion' q) = ValueQuestion' $ q & valueType .~ newValue
    set q = q

-- ------------------------------------------------------------------------------------------
qChangeIntegrationUuid :: (U.UUID -> Identity U.UUID) -> Question -> Identity Question
qChangeIntegrationUuid convert = Identity . set
  where
    newValue :: U.UUID
    newValue = runIdentity . convert $ U.nil
    set :: Question -> Question
    set (IntegrationQuestion' q) = IntegrationQuestion' $ q & integrationUuid .~ newValue
    set q = q

qChangeProps :: (M.Map String String -> Identity (M.Map String String)) -> Question -> Identity Question
qChangeProps convert = Identity . set
  where
    newValue :: M.Map String String
    newValue = runIdentity . convert $ M.empty
    set :: Question -> Question
    set (IntegrationQuestion' q) = IntegrationQuestion' $ q & props .~ newValue
    set q = q
