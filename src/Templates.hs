{-# LANGUAGE OverloadedStrings #-}
module Templates where

import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL (Text)

import Types(Bug(..))


bugList :: [Bug] -> TL.Text
bugList bugs = renderHtml $ do
    H.head $ do
        H.title "Bug list"
        --link to stylesheets
    H.body $ do
        H.div H.! A.id "header" $ "Bugs"
        H.p "This is some text"
        H.ul $ mapM_ (H.li . H.toMarkup . show) bugs
