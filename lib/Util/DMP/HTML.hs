module Util.DMP.HTML(dmp2html) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Model.KnowledgeModel.KnowledgeModel
import Model.FilledKnowledgeModel.FilledKnowledgeModel


dmp2html :: FilledKnowledgeModel -> String
dmp2html = renderHtml . fkm2html

fkm2html :: FilledKnowledgeModel -> H.Html
fkm2html km = html $ do
    H.head $
        H.title "DSW Data Management Plan"
    H.body $ do
        H.h1 . H.toHtml . _filledKnowledgeModelName $ km
        mapM_ chapter2html (_filledKnowledgeModelChapters km)

chapter2html :: FilledChapter -> H.Html
chapter2html chapter = html $
    H.div ! A.class_ "chapter" ! A.id (stringValue . show . _filledChapterUuid $ chapter) $ do
      H.h2 . H.toHtml . _filledChapterTitle $ chapter
      H.p . H.toHtml . _filledChapterText $ chapter
      mapM_ question2html (_filledChapterQuestions chapter)

question2html :: FilledQuestion -> H.Html
question2html question = html $
    H.div ! A.class_ "question" ! A.id (stringValue . show . _filledQuestionUuid $ question) $ do
      H.h3 . H.toHtml . _filledQuestionTitle $ question
      H.p . H.toHtml . _filledQuestionText $ question
      -- TODO: answer (text, option, items)
      -- TODO: experts
      -- TODO: references

answer2html :: FilledAnswer -> H.Html
answer2html answer = undefined

answerItem2html :: FilledAnswerItem -> H.Html
answerItem2html answerItem = undefined

expert2html :: Expert -> H.Html
expert2html expert = undefined

reference2html :: Reference -> H.Html
reference2html reference = undefined
