{-# LANGUAGE OverloadedStrings #-}
module Templates (bugList) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL (Text)
import Data.Maybe (isNothing, fromMaybe)

import Types(Bug(..), TestStatus)
import Utils(showMaybe, allValues, maybeEq)

bootstrap4 :: H.AttributeValue
bootstrap4 = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"

options :: (Show a, Eq a, Foldable t) => t a -> Maybe a -> H.Markup
options possible current = mapM_
    (\x -> H.option H.! A.value (H.toValue $ show x)
        H.!? (maybeEq x current, A.selected "selected")
        $ H.toMarkup $ show x) possible

optionsMaybe :: (Show a, Eq a, Foldable t ) => t a -> Maybe a -> H.Markup
optionsMaybe possible current = do
    H.option H.! A.value "-"
        H.!? (isNothing current, A.selected "selected") $ "-"
    options possible current

bugRow :: Bug -> H.Markup
bugRow bug =
    H.form
        H.! A.name (H.toValue $ jiraId bug)
        H.! A.action "/bugs"
        H.! A.method "post" $ do
        H.input H.! A.type_ "hidden" H.! A.name "jiraId"
            H.! A.value (H.toValue $ jiraId bug) H.! A.class_ "form-control"
        H.td $ H.toMarkup $ jiraId bug
        H.td $ H.a H.! A.href (H.toValue $ fromMaybe "" $ url bug)
            $ H.toHtml $ fromMaybe "" $ url bug
        H.td $ H.toMarkup $ showMaybe $ jiraStatus bug
        H.td $ H.input H.! A.type_ "text" H.! A.name "assignment"
            H.! A.class_ "form-control"
            H.! A.value (H.toValue $ fromMaybe "" $ assignment bug)
        H.td $ H.select H.! A.name "testStatus" H.! A.class_ "form-control" $
            optionsMaybe (allValues :: [TestStatus]) (testStatus bug)
        H.td $ H.input H.! A.type_ "text" H.! A.name "comments"
            H.! A.class_ "form-control"
            H.! A.value (H.toValue $ fromMaybe "" $ comments bug)
        H.td $ H.input H.! A.type_ "submit"
            H.! A.class_ "btn btn-primary"
            H.! A.value "submit"

bugList :: (Foldable t) => t Bug -> TL.Text
bugList bugs = renderHtml $ do
    H.head $ do
        H.title "Bug list"
        H.link H.! A.rel "stylesheet"
            H.! A.href bootstrap4

    H.body $
        H.div H.! A.class_ "container" $ do
            H.h1 "Bugs"
            H.table H.! A.class_ "table table-bordered" $ do
                H.thead H.! A.class_ "thead-dark" $ H.tr $ do
                    H.th "Jira Id"
                    H.th "URL"
                    H.th "Jira Status"
                    H.th "Assignee"
                    H.th "Test Status"
                    H.th "Comments"
                    H.th ""
                mapM_ (H.tr . bugRow) bugs
