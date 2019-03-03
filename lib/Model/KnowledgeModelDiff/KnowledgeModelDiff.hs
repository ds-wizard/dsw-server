module Model.KnowledgeModelDiff.KnowledgeModelDiff where

import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelDiff.DiffEvent

data KnowledgeModelDiff = KnowledgeModelDiff
  { _knowledgeModelDiffKnowledgeModel :: KnowledgeModel
  , _knowledgeModelDiffEvents :: [DiffEvent]
  }
