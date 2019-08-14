module Util.Lens where

import qualified Data.List as L
import Control.Lens

ap :: Functor f => Lens' entity [value] -> (value -> f value) -> entity -> f entity
ap accessor convert entity = fmap set (convert undefined)
  where
    set newValue = entity & accessor .~ ((entity ^. accessor) ++ [newValue])

del :: (Functor f, Eq value) => Lens' entity [value] -> (value -> f value) -> entity -> f entity
del accessor convert entity = fmap set (convert undefined)
  where
    set newValue = entity & accessor .~ (L.delete newValue (entity ^. accessor))

mAp :: Functor f => Lens' entity [value] -> (value -> f value) -> Maybe entity -> f (Maybe entity)
mAp accessor convert mEntity = fmap set (convert undefined)
  where
    set newValue =
      case mEntity of
        Just entity -> Just $ entity & accessor .~ ((entity ^. accessor) ++ [newValue])
        Nothing -> Nothing
