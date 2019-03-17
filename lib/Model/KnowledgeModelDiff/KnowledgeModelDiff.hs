module Model.KnowledgeModelDiff.KnowledgeModelDiff where

import Model.KnowledgeModel.KnowledgeModel
import Model.Event.Event

data KnowledgeModelDiff = KnowledgeModelDiff
  { _knowledgeModelDiffDiffKnowledgeModel :: KnowledgeModel
  , _knowledgeModelDiffDiffEvents :: [Event]
  , _knowledgeModelDiffPreviousKnowledgeModel :: KnowledgeModel
  }
