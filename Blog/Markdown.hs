{-# LANGUAGE OverloadedStrings #-}
module Blog.Markdown
    ( markdown
    ) where

import Data.Text (Text)
import Text.Blaze (Html)

import Text.Pandoc2.Reader.Markdown
import Text.Pandoc2.Shared
import Text.Pandoc2.Writer.HTML

markdown :: Text -> Html
markdown text =
    case markdownDoc poptions text of
      Nothing -> "Error!"
      Just doc ->
          docToHtml poptions doc

          