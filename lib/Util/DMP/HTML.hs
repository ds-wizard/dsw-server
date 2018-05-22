module Util.DMP.HTML(dmp2html) where

import Data.Maybe
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
      H.h4 . H.toHtml $ "Answer"
      qanswer2html question
      experts2html .  _filledQuestionExperts $ question
      references2html . _filledQuestionReferences $ question

qanswer2html :: FilledQuestion -> H.Html
qanswer2html question = case _filledQuestionQType question of
  QuestionTypeOptions -> answer2html . fromJust . _filledQuestionAnswerOption $ question
  QuestionTypeList -> answerItems2html . fromJust . _filledQuestionAnswerItems $ question
  _ -> H.div ! A.class_ "answer answer-simple" $
         H.toHtml . fromJust . _filledQuestionAnswerValue $ question

answer2html :: FilledAnswer -> H.Html
answer2html answer = H.div ! A.class_ "answer answer-option" ! A.id (stringValue . show . _filledAnswerUuid $ answer) $ do
  H.div ! A.class_ "label" $ H.toHtml . _filledAnswerLabel $ answer
  case _filledAnswerAdvice answer of
    Just advice -> H.p ! A.class_ "advice" $ H.toHtml advice
    Nothing -> H.toHtml ""
  H.div ! A.class_ "followups" $
    mapM_ question2html (_filledAnswerFollowUps answer)

answerItems2html :: [FilledAnswerItem] -> H.Html
answerItems2html answerItems = H.div ! A.class_ "answer answer-items" $
  mapM_ answerItem2html answerItems

answerItem2html :: FilledAnswerItem -> H.Html
answerItem2html answerItem = H.div ! A.class_ "answer answer-item" $ do
  H.div ! A.class_ "item" $ do
    H.span ! A.class_ "title" $ H.toHtml . _filledAnswerItemTitle $ answerItem
    H.span ! A.class_ "value" $ H.toHtml "" -- TODO: missing value
  H.div ! A.class_ "followups" $
    mapM_ question2html (_filledAnswerItemQuestions answerItem)

experts2html :: [Expert] -> H.Html
experts2html [] = H.toHtml ""
experts2html experts = H.div ! A.class_ "experts" $ do
  H.h4 . H.toHtml $ "Experts"
  H.ul ! A.class_ "experts-list" $
    mapM_ expert2html experts
      where expert2html expert = H.li ! A.class_ "expert" ! A.id (stringValue . show . _expertUuid $ expert) $ do
                                   H.span ! A.class_ "name" $ H.toHtml . _expertName $ expert
                                   H.preEscapedToMarkup "&nbsp;"
                                   H.span ! A.class_ "email" $ H.toHtml . _expertEmail $ expert

references2html :: [Reference] -> H.Html
references2html [] = H.toHtml ""
references2html references = H.div ! A.class_ "references" $ do
  H.h4 . H.toHtml $ "References"
  H.ul ! A.class_ "references-list" $
    mapM_ reference2html references
      where reference2html reference = H.li ! A.class_ "reference reference-dmpbook" ! A.id (stringValue . show . _referenceUuid $ reference) $ do
                                   H.span ! A.class_ "dmpbook-chapter" $ H.toHtml . _referenceChapter $ reference
