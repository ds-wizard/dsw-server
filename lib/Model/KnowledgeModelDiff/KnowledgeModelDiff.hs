module Model.KnowledgeModelDiff.KnowledgeModelDiff where

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelDiff = KnowledgeModelDiff
  { _knowledgeModelDiffDiffKnowledgeModel :: KnowledgeModel
  , _knowledgeModelDiffDiffEvents :: [Event]
  , _knowledgeModelDiffPreviousKnowledgeModel :: KnowledgeModel
  }
