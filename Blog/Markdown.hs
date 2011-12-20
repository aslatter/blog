{-# LANGUAGE OverloadedStrings #-}
module Blog.Markdown
    ( markdown
    , markdownCssTagName
    , markdownCss
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Blaze (Html)
import Text.Highlighting.Kate (defaultHighlightingCss)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc
import Text.Templating.Heist
import Text.XmlHtml (Node(..))

markdown :: Text -> Html
markdown text =
    case readMarkdown defaultParserState (Text.unpack text) of
      doc -> writeHtml writeOpts doc

writeOpts :: WriterOptions
writeOpts =
    defaultWriterOptions
      { writerStandalone = False
      , writerHtml5 = False
      }

markdownCssTagName :: Text
markdownCssTagName = "highlightingStyle"

markdownCss :: Monad m => Splice m
markdownCss = return [Element "style" [] [TextNode (Text.pack defaultHighlightingCss)]]
