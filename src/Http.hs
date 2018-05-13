{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}

-- |
-- Module      : Network.Wreq.Internal.Types
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client types.

module Http (
      QueryString
    , HttpOptions(..)
    , defaultHttpOptions
    , fetch
    , asJSON
    , asValue
    , Response
    , HTTP.responseBody
) where

import Control.Monad (unless)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON, eitherDecode', Value, Object)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Simple
import Network.HTTP.Conduit (applyBasicAuth, setQueryString)

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

fetch :: HttpOptions -> IO (Response L.ByteString)
fetch options = httpLBS request
    where
        request = buildHttpRequest options
    
asJSON :: (MonadThrow m, FromJSON a) => 
          Response L.ByteString -> m (Response a)
asJSON resp = do
    let contentType = fst . S.break (==59) . fromMaybe "unknown" .
                      lookup "Content-Type" . HTTP.responseHeaders $ resp
    unless ("application/json" `S.isPrefixOf` contentType
            || ("application/" `S.isPrefixOf` contentType && "+json" `S.isSuffixOf` contentType)) $
        throwM . JSONError $ "content type of response is " ++ show contentType
    case eitherDecode' (HTTP.responseBody resp) of
        Left err -> throwM (JSONError err)
        Right val -> return (fmap (const val) resp)

asValue :: (MonadThrow m) => Response L.ByteString -> m (Response Value)
asValue = asJSON

type QueryValuePair = (C.ByteString, Maybe C.ByteString)

type QueryString = [QueryValuePair]

type Auth = (C.ByteString, C.ByteString)

data HttpOptions = 
    HttpOptions { auth        :: Auth
                , host        :: C.ByteString 
                , method      :: C.ByteString
                , path        :: C.ByteString
                , queryString :: QueryString
                , secure      :: Bool
                , port        :: Int }
                deriving (Show)

defaultHttpOptions :: HttpOptions
defaultHttpOptions = HttpOptions ("", "") "" "GET" "" [] False 80
                                
buildHttpRequest :: HttpOptions -> Request
buildHttpRequest (HttpOptions auth host method path qs secure port) = 
    setRequestMethod method
    $ setRequestHost host
    $ applyBasicAuth (fst auth) (snd auth)
    $ setRequestPath path
    $ setQueryString qs
    $ setRequestSecure secure
    $ setRequestPort port
    $ defaultRequest
