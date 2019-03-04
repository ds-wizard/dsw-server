module Database.BSON.KnowledgeModelDiff.DiffEvent where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Model.KnowledgeModelDiff.DiffEvent

instance ToBSON DiffEvent where
  toBSON state = [ "event" BSON.=: "edited"]

instance FromBSON DiffEvent where
  fromBSON doc = return . NodeEdited $ ""
