{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai
  (
    Client(..),
    Info(..),
    authorize,
    info
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
import           Network.Utilities
import           Network.Wreq

-- The Client data type has two constructors. The first should be used
-- when constructing a client with an access token. The second constructor
-- should be used when passing in an application's client id and client secret.
data Client = Client String | App String String deriving (Show)

-- Turn our authorized Client into an Authorization header
authHeader :: Client -> Options
authHeader (Client token) = defaults & header "Authorization" .~ [packed]
  where auth = "Bearer " ++ token
        packed = BStrict.pack auth
authHeader _ = defaults

-- The Info data type is used as a response from the
-- /info endpoint. This type contains information about the
-- various usage limits for the API.
data Info = Info {
  maxBatchSize      :: Integer,
  maxImageSize      :: Integer,
  minImageSize      :: Integer,
  maxImageBytes     :: Integer,
  maxVideoBatchSize :: Integer,
  maxVideoSize      :: Integer,
  minVideoSize      :: Integer,
  maxVideoBytes     :: Integer,
  maxVideoDuration  :: Integer
} deriving (Show)

-- Convert a Map of Strings and Values into an Info type
toInfo :: Obj -> Info
toInfo origObj = Info mbs maxis minis maxib mvbs maxvs minvs mvb mvd
  where obj = getMap "results" origObj
        mbs   = getInt' "max_batch_size"       obj
        maxis = getInt' "max_image_size"       obj
        minis = getInt' "min_image_size"       obj
        maxib = getInt' "max_image_bytes"      obj
        mvbs  = getInt' "max_video_batch_size" obj
        maxvs = getInt' "max_video_size"       obj
        minvs = getInt' "min_video_size"       obj
        mvb   = getInt' "max_video_bytes"      obj
        mvd   = getInt' "max_video_duration"   obj

--------------------
---- API Routes ----
--------------------
-- For authentication
tokenUrl = "https://api.clarifai.com/v1/token/"
-- For API info
infoUrl  = "https://api.clarifai.com/v1/info/"

-- Authorize an application
-- Sends a POST request to Clarifai's authentication endpoint.
-- If we have a Client, we just return the client because I'm assuming
-- the client was constructed with a valid access token. If we have an App,
-- we would POST the client ID and client secret to Clarifai to get an
-- access token.
authorize :: Client -> IO (Either Errors Client)
authorize (Client token) = return (Right (Client token))
authorize (App clientID clientSecret) = resp
  where params = ["client_id" := clientID,
                      "client_secret" := clientSecret,
                      "grant_type" := BS.pack "client_credentials"]
        key = "access_token"
        resp = do (status, body) <- processRequest $ postWith' tokenUrl params
                  let code = status ^. statusCode in
                    if code /= 200 then
                      return (Left (code, apiErr code body))
                    else
                      return (Right (Client $ getString key body))

-- Gets the Clarifai API limits and information.
-- Sends a get request to the /info endpoint and encapsulates
-- the results into an Info type.
info :: Client -> IO (Either Errors Info)
info (App _ _) = return (Left (0, "You have not authorized your app yet."))
info client = resp
  where opts = authHeader client
        resp = do (status, body) <- processRequest $ getWith opts infoUrl
                  let code = status ^. statusCode in
                    if code /= 200 then
                      return (Left (code, apiErr code body))
                    else
                      return (Right (toInfo body))
