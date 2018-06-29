module Model.DataManagementPlan.DataManagementPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.FilledKnowledgeModel.FilledKnowledgeModel

data DataManagementPlanFormat
  = JSON
  | HTML
  | PDF
  | LaTeX
  | Docx
  | ODT
  | Markdown
  deriving (Show, Read, Eq, Enum, Bounded, Generic)

data DataManagementPlan = DataManagementPlan
  { _dataManagementPlanUuid :: U.UUID
  , _dataManagementPlanQuestionnaireUuid :: String
  , _dataManagementPlanFilledKnowledgeModel :: FilledKnowledgeModel
  , _dataManagementPlanCreatedAt :: UTCTime
  , _dataManagementPlanUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq DataManagementPlan where
  a == b =
    _dataManagementPlanUuid a == _dataManagementPlanUuid b &&
    _dataManagementPlanQuestionnaireUuid a == _dataManagementPlanQuestionnaireUuid b &&
    _dataManagementPlanFilledKnowledgeModel a == _dataManagementPlanFilledKnowledgeModel b

formatExtension :: DataManagementPlanFormat -> String
formatExtension JSON      = ".json"
formatExtension HTML      = ".html"
formatExtension LaTeX     = ".tex"
formatExtension Markdown  = ".md"
formatExtension Docx      = ".docx"
formatExtension ODT       = ".odt"
formatExtension PDF       = ".pdf"
--formatExtension RTF       = ".rtf"
--formatExtension RST       = ".rst"
--formatExtension AsciiDoc  = ".adoc"
--formatExtension DokuWiki  = ".dw.txt"
--formatExtension MediaWiki = ".mw.txt"
--formatExtension EPUB2     = ".epub"
--formatExtension EPUB3     = ".epub"
formatExtension _         = ""

stringToFormat :: String -> Maybe DataManagementPlanFormat
stringToFormat "json"      = Just JSON
stringToFormat "html"      = Just HTML
stringToFormat "latex"     = Just LaTeX
stringToFormat "html"      = Just Markdown
stringToFormat "docx"      = Just Docx
stringToFormat "odt"       = Just ODT
stringToFormat "pdf"       = Just PDF
-- stringToFormat "rtf"       = Just RTF
-- stringToFormat "rst"       = Just RST
-- stringToFormat "asciidoc"  = Just AsciiDoc
-- stringToFormat "dokuwiki"  = Just DokuWiki
-- stringToFormat "mediawiki" = Just MediaWiki
-- stringToFormat "epub2"     = Just EPUB2
-- stringToFormat "epub3"     = Just EPUB3
stringToFormat _           = Nothing
