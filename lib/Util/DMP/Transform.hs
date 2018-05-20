module Util.DMP.Transform where

import Data.Text
import Text.Pandoc as Pandoc

import Util.DMP.HTML (dmp2html)
import Model.FilledKnowledgeModel.FilledKnowledgeModel

dmpTransform :: DMPExportType -> FilledKnowledgeModel -> Maybe Text
dmpTransform HTML  dmp = Just . pack . dmp2html $ dmp
dmpTransform eType dmp = case Pandoc.readHtml def (dmp2html dmp) of
                            Left _ -> Nothing
                            Right pandoc -> Just . pack . pandocWriter eType def $ pandoc


data DMPExportType = HTML
                   | LaTeX
                   | RTF
                   | RST
                   | Markdown
                   | MediaWiki
                   deriving (Show, Read, Enum, Bounded, Eq)

exportType2String :: DMPExportType -> String
exportType2String HTML = "html"
exportType2String LaTeX = "latex"
exportType2String RTF = "rtf"
exportType2String RST = "rst"
exportType2String Markdown = "markdown"
exportType2String MediaWiki = "mediawiki"

pandocWriter :: DMPExportType -> (Pandoc.WriterOptions -> Pandoc.Pandoc -> String)
pandocWriter HTML = Pandoc.writeHtmlString
pandocWriter LaTeX = Pandoc.writeLaTeX
pandocWriter RTF = Pandoc.writeRTF
pandocWriter RST = Pandoc.writeRST
pandocWriter Markdown = Pandoc.writeMarkdown
pandocWriter MediaWiki = Pandoc.writeMediaWiki
pandocWriter _ = error "Undefined Pandoc writer"
