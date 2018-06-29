module Model.DataManagementPlan.DataManagementPlanHelpers where

import Model.DataManagementPlan.DataManagementPlan


formatExtension :: DataManagementPlanFormat -> String
formatExtension JSON      = ".json"
formatExtension HTML      = ".html"
formatExtension LaTeX     = ".tex"
formatExtension Markdown  = ".md"
formatExtension Docx      = ".docx"
formatExtension ODT       = ".odt"
formatExtension PDF       = ".pdf"
formatExtension RTF       = ".rtf"
formatExtension RST       = ".rst"
formatExtension AsciiDoc  = ".adoc"
formatExtension DokuWiki  = ".dw.txt"
formatExtension MediaWiki = ".mw.txt"
formatExtension EPUB2     = ".epub"
formatExtension EPUB3     = ".epub"
formatExtension _         = ""

stringToFormat :: String -> Maybe DataManagementPlanFormat
stringToFormat "json"      = Just JSON
stringToFormat "html"      = Just HTML
stringToFormat "latex"     = Just LaTeX
stringToFormat "html"      = Just Markdown
stringToFormat "docx"      = Just Docx
stringToFormat "odt"       = Just ODT
stringToFormat "pdf"       = Just PDF
stringToFormat "rtf"       = Just RTF
stringToFormat "rst"       = Just RST
stringToFormat "asciidoc"  = Just AsciiDoc
stringToFormat "dokuwiki"  = Just DokuWiki
stringToFormat "mediawiki" = Just MediaWiki
stringToFormat "epub2"     = Just EPUB2
stringToFormat "epub3"     = Just EPUB3
stringToFormat _           = Nothing
