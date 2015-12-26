{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai
  (
    Client(..),
    authorize
  ) where

import qualified Control.Exception          as E
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString            as BL
import qualified Data.ByteString.Char8      as BStrict
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either
import qualified Data.Map.Lazy              as Map
import qualified Data.Text                  as T
import qualified Network.HTTP.Client        as Net
import           Network.Wreq

-- The Client data type has two constructors. The first should be used
-- when constructing a client with an access token. The second constructor
-- should be used when passing in an application's client id and client secret.
data Client = Client T.Text | App String String deriving (Show)

-- needed?
type Url = String

-- needed?
type ApiResponse = Response (Map.Map String Value)

type Resp = Response BS.ByteString
type Errors = (Int, String)

-- API Routes
tokenUrl = "https://api.clarifai.com/v1/token/"

-- Authorize an application
-- Sends a POST request to Clarifai's authentication endpoint.
-- If we have a Client, we just return the client because I'm assuming
-- the client was constructed with a valid access token. If we have an App,
-- we would POST the client ID and client secret to Clarifai to get an
-- access token.
authorize :: Client -> IO (Either Errors Client)
authorize (Client token) = return (Right (Client token))
authorize (App clientID clientSecret) = resp
  where authParams = ["client_id" := clientID,
                      "client_secret" := clientSecret,
                      "grant_type" := BS.pack "client_credentials"]
        handler (Net.StatusCodeException s _ _) = return (Left (code, msg))
          where code = s ^. statusCode
                msg = s ^. statusMessage
        resp = do (status, body) <- processRequest $ postWith' tokenUrl authParams
                  let code = status ^. statusCode in
                    if code /= 200 then
                      return (Left (code, show $ status ^. statusMessage))
                    else
                      return (Right (Client $ body ^. key "access_token" . _String))

-- Process the results of a request
-- Returns an IO Tuple of the status and response body.
processRequest :: IO Resp -> IO (Status, BS.ByteString)
processRequest response = do  r <- response
                              return (r ^. responseStatus, r ^. responseBody)

-- custom postWith that overrides default checkStatus functionality.
-- no longer returns an error on non-2** status codes.
postWith' = postWith (set checkStatus (Just $ \_ _ _ -> Nothing) defaults)

-- Turn our authorized Client into an Authorization header
clientToAuthHeader :: Client -> Options
clientToAuthHeader (Client token) = defaults & header "Authorization" .~ [packed]
  where unpacked = T.unpack token
        auth = "Bearer " ++ unpacked
        packed = BStrict.pack auth
clientToAuthHeader _ = defaults
