
module Blog.Forms where

import Blog.Core

import Data.Monoid
import Text.Blaze.Html5 (Html, toHtml)
import Text.Digestive hiding (required)
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack

type AppForm a = HappstackForm App Html BlazeFormHtml a

mkRequired :: (Eq a, Monoid a) => String -> AppForm a -> AppForm a
mkRequired msg =
    flip validate (required $ toHtml msg)

required :: (Monoid a, Eq a, Monad m) =>
            errMsg
         ->  Validator m errMsg a
required e = check e (/= mempty)

