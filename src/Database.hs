module Database where

import Database.SQLite.Simple (NamedParam(..),
    queryNamed, query_, execute, executeNamed, withConnection, changes)
import qualified Data.Text.Lazy as TL (Text)

import Types (Bug (..), BugUpdate (..))

getBugs :: String -> IO [Bug]
getBugs db = withConnection db $ \conn ->
                query_ conn "SELECT * FROM bugs"

updateBug :: String -> BugUpdate -> IO (Either TL.Text Int)
updateBug db bug =
    withConnection db $ \conn -> do
        executeNamed conn
            "UPDATE bugs SET assignment = :a, test_status = :t, comments = :c WHERE jira_id = :j"
                [":a" := newAssignment bug, ":t" := newTestStatus bug,
                ":c" := newComments bug, ":j" := newJiraId bug]
        numRowsChanged <- changes conn
        if numRowsChanged /= 1
            then return (Left "Database error")
            else return (Right 1)

getBug :: String -> TL.Text -> IO (Maybe Bug)
getBug db bug = withConnection db $ \conn -> do
    r <- queryNamed conn
        "SELECT * FROM bugs WHERE jira_id = :id" [":id" := bug] :: IO [Bug]
    case r of
        (b:_) -> return (Just b)
        _ -> return Nothing

deleteBug :: String -> TL.Text -> IO (Either TL.Text Int)
deleteBug db bug = withConnection db $ \conn -> do
    executeNamed conn "DELETE FROM bugs WHERE jira_id = :id" [":id" := bug]
    numRowsChanged <- changes conn
    if numRowsChanged /= 1
        then return (Left "Database error")
        else return (Right 1)

addBug :: String -> Bug -> IO (Either TL.Text Int)
addBug db bug = withConnection db $ \conn -> do
    execute conn "INSERT INTO bugs (jira_id, url, jira_status, assignment, test_status, comments) values (?, ?, ?, ?, ?, ?)" bug
    numRowsChanged <- changes conn
    if numRowsChanged /= 1
        then return (Left "Database error")
        else return (Right 1)

