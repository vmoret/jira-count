module Options (
      Options(..)
    , parseOptions
) where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
    { user       :: String
    , password   :: String
    , jql        :: String
    , host       :: String }
    deriving (Show)

parseOptions :: IO Options
parseOptions = execParser opts
    where
        opts = info (options <**> helper)
          ( fullDesc
         <> progDesc "Counts JIRA issues"
         <> header "jira-counts - counts the total matches for JQL search on JIRA server" )

options :: Parser Options
options = Options
    <$> strOption
        ( long "user"
        <> short 'u'
        <> metavar "USER"
        <> help "JIRA username" )
    <*> strOption
        ( long "password"
        <> short 'p'
        <> metavar "PASSWORD"
        <> help "JIRA password" )
    <*> strOption
        ( long "jql"
        <> metavar "JQL-STRING"
        <> help "JIRA JQL query string" )
    <*> strOption
        ( long "host"
        <> metavar "HOST"
        <> value "itrack.barco.com"
        <> help "JIRA server host name" )