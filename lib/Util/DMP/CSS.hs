module Util.DMP.CSS(dmpCSS) where

import Clay

-- | CSS style for generate HTML DMP
-- TODO: finish the style this is just dummy
dmpCSS = renderWith pretty [] $ do
  h1 ? color red
  h2 ? color orange
