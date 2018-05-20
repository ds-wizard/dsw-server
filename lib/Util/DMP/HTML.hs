module Util.DMP.HTML(dmp2html) where

import Data.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Model.FilledKnowledgeModel.FilledKnowledgeModel


dmp2html :: FilledKnowledgeModel -> String
dmp2html = renderHtml . fkm2html

fkm2html :: FilledKnowledgeModel -> H.Html
fkm2html fkm = html $ do
    H.head $ do
        H.title "DSW Data Management Plan"
    H.body $ do
        H.h1 . H.toHtml . _filledKnowledgeModelName $ fkm
        H.text "Here is the content of DSW-generated DMP"
