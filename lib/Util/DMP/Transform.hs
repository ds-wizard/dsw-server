module Util.DMP.Transform
       ( dmpTransform
       , DMPExportType(..)
       ) where

import qualified Data.ByteString as B

import           Text.FromHTML
import           Util.DMP.HTML (dmp2html)
import           Model.FilledKnowledgeModel.FilledKnowledgeModel


-- | Enumeration of supported export document types
type DMPExportType = ExportType

-- | Exposed interface for tranforming filled KM to document (as UTF-8 ByteString)
dmpTransform :: DMPExportType -> FilledKnowledgeModel -> Maybe B.ByteString
dmpTransform exportType fkm = fromHTML exportType (dmp2html fkm)

-- | Exposed interface for tranforming filled KM to document (as UTF-8 ByteString)
dmpTransformTo :: String -> FilledKnowledgeModel -> Maybe B.ByteString
dmpTransformTo strType fkm = fromHTML (read strType) (dmp2html fkm)

exportTypes :: [DMPExportType]
exportTypes = [minBound..maxBound]

exportTypesStrings :: [String]
exportTypesStrings = map show exportTypes
