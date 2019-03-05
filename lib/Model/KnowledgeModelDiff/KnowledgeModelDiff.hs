module Model.KnowledgeModelDiff.KnowledgeModelDiff where

import Model.KnowledgeModel.KnowledgeModel
import Model.Event.Event

data KnowledgeModelDiff = KnowledgeModelDiff
  { _knowledgeModelDiffKnowledgeModel :: KnowledgeModel
  , _knowledgeModelDiffEvents :: [Event]
  }
