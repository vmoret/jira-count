{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Jira 
    ( Issue
    , SearchResults
    , searchIssues
    , countIssues ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Object)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Control.Arrow (second)

import Http ( fetch, defaultHttpOptions, QueryString, HttpOptions(..)
            , asJSON, responseBody, Response)

data SearchParams =
    SearchParams { qJQL           :: String
                 , qStartAt       :: Int
                 , qMaxResults    :: Int
                 , qValidateQuery :: Bool
                 , qFields        :: [String]
                 , qExpands       :: [String] }
                 deriving (Show)

defaultSearchParams :: SearchParams
defaultSearchParams = SearchParams "" 0 50 True ["summary"] ["*navigable"]

type Issue = Object

data SearchResults = 
    SearchResults { expand        :: String
                  , startAt       :: Int
                  , maxResults    :: Int
                  , total         :: Int
                  , issues        :: [Issue] } 
                  deriving (Show, Generic)

instance FromJSON SearchResults where

searchIssues :: String -> String -> String -> SearchParams -> IO SearchResults
searchIssues hostname user password params = 
    responseBody <$> (asJSON =<< fetch httpOptions)
    where
        httpOptions = buildHttpOptions hostname user password params

countIssues :: String -> String -> String -> String -> IO Int
countIssues hostname user password jql = 
    total <$> (searchIssues hostname user password params)
    where params = defaultSearchParams { qJQL = jql, qMaxResults = 1 }

buildHttpOptions :: String -> String -> String -> SearchParams -> HttpOptions
buildHttpOptions hostname user password params =
            defaultHttpOptions { auth = (C.pack user, C.pack password)
                               , host = C.pack hostname
                               , path = "/rest/api/latest/search"
                               , queryString = (buildQueryString params)
                               , secure = True
                               , port = 443 }

buildQueryString :: SearchParams -> QueryString
buildQueryString (SearchParams jql startAt maxResults validateQuery fields expands) = 
    map (second Just) pairs
    where
        i2c i = C.pack $ show i
        join xs = C.intercalate "," [C.pack x | x <- xs]
        pairs = [ ("jql", C.pack jql)
                , ("maxResults", i2c maxResults)
                , ("startAt", i2c startAt)
                , ("validateQuery", i2c validateQuery)
                , ("fields", join fields)
                , ("expands", join expands) ]
