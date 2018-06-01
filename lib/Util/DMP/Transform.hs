module Util.DMP.Transform
       ( dmpTransform
       , DMPExportType(..)
       ) where

import Data.Text as T
import Data.Text.Encoding as E
import Data.ByteString as B
import Text.Pandoc as Pandoc

import Util.DMP.HTML (dmp2html)
import Model.FilledKnowledgeModel.FilledKnowledgeModel


-- | Exposed interface for tranforming filled KM to document (as UTF-8 ByteString)
dmpTransform :: DMPExportType -> FilledKnowledgeModel -> Maybe ByteString
dmpTransform _ _ = Just . E.encodeUtf8 $ "To be implemented ..."

dmpTransform' :: DMPExportType -> FilledKnowledgeModel -> Maybe Text
dmpTransform' HTML  dmp = Just . T.pack . dmp2html $ dmp
dmpTransform' eType dmp = case Pandoc.readHtml def (dmp2html dmp) of
                            Left _ -> Nothing
                            Right pandoc -> Just . T.pack . pandocWriter eType def $ pandoc

-- | Enumeration of supported export document types
data DMPExportType = HTML
                   | LaTeX
                   | RTF
                   | RST
                   | Markdown
                   | MediaWiki
                   deriving (Show, Read, Enum, Bounded, Eq)

pandocWriter :: DMPExportType -> (Pandoc.WriterOptions -> Pandoc.Pandoc -> String)
pandocWriter HTML = Pandoc.writeHtmlString
pandocWriter LaTeX = Pandoc.writeLaTeX
pandocWriter RTF = Pandoc.writeRTF
pandocWriter RST = Pandoc.writeRST
pandocWriter Markdown = Pandoc.writeMarkdown
pandocWriter MediaWiki = Pandoc.writeMediaWiki
pandocWriter _ = error "Undefined Pandoc writer"
