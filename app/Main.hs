module Main where

import Jira (countIssues)
import Options (parseOptions, host, user, password, jql)

main :: IO ()
main = do
    opts <- parseOptions
    count <- countIssues (host opts) (user opts) (password opts) (jql opts)
    print count
