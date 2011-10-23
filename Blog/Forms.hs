{-# LANGUAGE OverloadedStrings #-}
module Blog.Forms where

import Blog.Core
import Blog.Templates

import Data.Monoid
import Data.Text (Text)
import Text.Blaze.Html5 (Html, toHtml, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive hiding (required)
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Happstack.Server (finishWith)

type AppForm a = HappstackForm App Html BlazeFormHtml a

mkRequired :: (Eq a, Monoid a) => String -> AppForm a -> AppForm a
mkRequired msg =
    flip validate (required $ toHtml msg)

required :: (Monoid a, Eq a, Monad m) =>
            errMsg
         ->  Validator m errMsg a
required e = check e (/= mempty)


-- | Either display the given form to the client
-- in a single response, or return the posted
-- form value.
handleForm :: Text -- ^ page title
           -> AppForm a
           -> App a
handleForm title form = do
  r <- eitherHappstackForm form "form"
  case r of
    Left formV -> do
        let (renderedForm,_) = renderFormHtml formV
        response <-
            renderBlaze
               [("pageTitle", title)]
               "_layout"
               $ H.form ! A.method (toValue ("POST"::String)) $
                  do
                    renderedForm
                    H.input ! A.type_ "submit"
        finishWith response
    Right val -> return val
