module Service.DataManagementPlan.Templates.Mapper where

import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Maybe as MB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.FromHTML as FromHTML

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Common.Localization
import Model.DataManagementPlan.DataManagementPlan
import LensesConfig
import Service.DataManagementPlan.Templates.Html

-- | Enumeration of supported export document types
type DMPExportType = FromHTML.ExportType

strToBSL :: String -> BSL.ByteString
strToBSL = BSL.fromStrict . E.encodeUtf8 . T.pack

toHTML :: DataManagementPlanDTO -> BSL.ByteString
toHTML = strToBSL . mkHTMLString

toFormat :: DataManagementPlanFormat -> DataManagementPlanDTO -> BSL.ByteString
toFormat format = MB.fromMaybe (strToBSL _ERROR_SERVICE_DMP__TRANSFORMATION_FAILED) . toType' (formatToType format)
  where
    toType' :: Maybe DMPExportType -> DataManagementPlanDTO -> Maybe BSL.ByteString
    toType' (Just eType) dmp = fmap BSL.fromStrict . FromHTML.fromHTML eType . mkHTMLString $ dmp
    toType' _ _              = Just (strToBSL _ERROR_SERVICE_DMP__UKNOWN_FORMAT)

mkHTMLString :: DataManagementPlanDTO -> String
mkHTMLString dmp = dmp2html $ dmp ^. filledKnowledgeModel

formatToType :: DataManagementPlanFormat -> Maybe DMPExportType
formatToType HTML      = Just FromHTML.HTML
formatToType LaTeX     = Just FromHTML.LaTeX
formatToType Markdown  = Just FromHTML.Markdown
formatToType Docx      = Just FromHTML.Docx
formatToType ODT       = Just FromHTML.ODT
formatToType PDF       = Just FromHTML.PDF
formatToType RTF       = Just FromHTML.RTF
formatToType RST       = Just FromHTML.RST
formatToType AsciiDoc  = Just FromHTML.AsciiDoc
formatToType DokuWiki  = Just FromHTML.DokuWiki
formatToType MediaWiki = Just FromHTML.MediaWiki
formatToType EPUB2     = Just FromHTML.EPUB2
formatToType EPUB3     = Just FromHTML.EPUB3
formatToType _         = Nothing
