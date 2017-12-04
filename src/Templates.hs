{-# LANGUAGE OverloadedStrings #-}
module Templates where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy as TL (Text)
import Data.Maybe (maybe)

import Types(Bug(..))

showMaybe :: (Show s) => Maybe s -> String
showMaybe = maybe "" show

bugRow :: Bug -> H.Markup
bugRow bug = do
    H.td $ H.toMarkup $ jiraId bug
    H.td $ H.a H.! A.href (H.toValue $ maybe "" id $ url bug) $ H.toHtml $ showMaybe $ url bug
    H.td $ H.toMarkup . showMaybe $ jiraStatus bug
    H.td $ H.input H.! A.type_ "text" H.! A.name "assignment" H.! A.value (H.toValue $ showMaybe $ assignment bug)
    H.td $ H.toMarkup . showMaybe $ testStatus bug
    H.td $ H.input H.! A.type_ "text" H.! A.name "comments" H.! A.value (H.toValue $ showMaybe $ comments bug)

bugList :: [Bug] -> TL.Text
bugList bugs = renderHtml $ do
    H.head $ do
        H.title "Bug list"
        H.link H.! A.rel "stylesheet" H.! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css" -- H.! A.integrity "sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb" H.! A.crossorigin "anonymous"

    H.body $ do
        H.div $ H.h1 "Bugs"
        H.table H.! A.class_ "table table-bordered" $ do
            H.thead H.! A.class_ "thead-dark" $ H.tr $ do
                H.th "Jira Id"
                H.th "URL"
                H.th "Jira Status"
                H.th "Assignee"
                H.th "Test Status"
                H.th "Comments"
            mapM_ (H.tr . bugRow) bugs
