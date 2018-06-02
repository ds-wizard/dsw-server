module Util.DMP.Transform
       ( dmpTransform
       , DMPExportType(..)
       ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Writers as PandocWriters

import           Util.DMP.HTML (dmp2html)
import           Model.FilledKnowledgeModel.FilledKnowledgeModel

-- | Exposed interface for tranforming filled KM to document (as UTF-8 ByteString)
dmpTransform :: DMPExportType -> FilledKnowledgeModel -> Maybe B.ByteString
dmpTransform HTML fkm = Just . E.encodeUtf8 . T.pack . dmp2html $ fkm
dmpTransform et fkm = case mpd of
                        Just pd -> eitherToMaybe . Pandoc.runPure $ writer Pandoc.def pd
                        _       -> Nothing
  where
    mpd :: Maybe Pandoc.Pandoc
    mpd = eitherToMaybe . Pandoc.runPure $ Pandoc.readHtml Pandoc.def . T.pack . dmp2html $ fkm
    writer = dmpWriter et

-- | Enumeration of supported export document types
data DMPExportType = HTML
                   | LaTeX
                   | RTF
                   | RST
                   | Markdown
                   | AsciiDoc
--                   | Docx     -- (faulty at the moment)
                   deriving (Show, Read, Enum, Bounded, Eq)

-- -----------------------------------------------------------------------------

type DMPWriter = (Pandoc.WriterOptions -> Pandoc.Pandoc -> Pandoc.PandocPure B.ByteString)

-- | Helper function to translate Either to Maybe
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

-- | Select DMPWriter based on given DMPExportType
dmpWriter :: DMPExportType -> DMPWriter
dmpWriter = dmpizeWriter . pandocWriter
  where
    dmpizeWriter :: Pandoc.Writer Pandoc.PandocPure -> DMPWriter
    dmpizeWriter (Pandoc.TextWriter        tw) = \opts pd -> E.encodeUtf8 <$> tw opts pd
    dmpizeWriter (Pandoc.ByteStringWriter bsw) = \opts pd -> BL.toStrict <$> bsw opts pd


pandocWriter :: DMPExportType -> Pandoc.Writer Pandoc.PandocPure
pandocWriter HTML     = Pandoc.TextWriter PandocWriters.writeHtml5String
pandocWriter LaTeX    = Pandoc.TextWriter PandocWriters.writeLaTeX
pandocWriter RTF      = Pandoc.TextWriter PandocWriters.writeRTF
pandocWriter RST      = Pandoc.TextWriter PandocWriters.writeRST
pandocWriter Markdown = Pandoc.TextWriter PandocWriters.writeMarkdown
pandocWriter AsciiDoc = Pandoc.TextWriter PandocWriters.writeAsciiDoc
--pandocWriter Docx     = Pandoc.ByteStringWriter PandocWriters.writeDocx
