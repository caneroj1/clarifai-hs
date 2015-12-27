{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai
  (
    Client(..),
    TagSet(..),
    verifyImageBatchSize,
    verifyVideoBatchSize,
    verifyFiles,
    authorize,
    info,
    tag
  ) where

import qualified Control.Exception          as E
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString            as BL
import qualified Data.ByteString.Char8      as BStrict
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either
import           Data.List
import qualified Data.Map.Lazy              as Map
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Network.HTTP
import qualified Network.HTTP.Client        as Net
import           Network.Utilities
import           Network.Wreq
import           System.EasyFile

-- The Client data type has two constructors. The first should be used
-- when constructing a client with an access token. The second constructor
-- should be used when passing in an application's client id and client secret.
data Client = Client String | App String String deriving (Show)

-- Turn our authorized Client into an Authorization header
authHeader :: Client -> Options
authHeader (Client token) = defaults' & header "Authorization" .~ [packed]
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

-- A TagSet represents a single result from the Tag endpoint.
-- Each result has a unique docid, but they can also have local IDs
-- if those are provided. They also have words and probabilities.
type Tag = (String, Double)
data TagSet = TagSet {
  docID   :: Integer,
  localID :: String,
  tags    :: V.Vector Tag
} deriving (Show)

getTags :: HObj -> V.Vector Tag
getTags o = V.zip classes probs
  where classes = V.map value2String (getVec' "classes" o)
        probs = V.map value2Double (getVec' "probs" o)

objToTagSet :: HObj -> TagSet
objToTagSet o = TagSet docID localID tags
  where docID = getInt' "docid" o
        localID = getString' "local_id" o
        tags = getTags (getMap' "tag" $ getMap' "result" o)

--------------------
---- API Routes ----
--------------------
-- For authentication
tokenUrl  = "https://api.clarifai.com/v1/token/"
-- For API info
infoUrl   = "https://api.clarifai.com/v1/info/"
-- Tag images/videos
tagUrl    = "https://api.clarifai.com/v1/tag/"

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

-- TODO: support localIDs
-- TODO: use partFileSource?
-- Utilizes the tag endpoint of the clarifai API to tag
-- multiple files from the local file system.
tag :: Client -> [FilePath] -> IO (Either Errors (V.Vector TagSet))
tag (App _ _) _ = return (Left (0, "You have not authorized your app yet."))
tag c fs = resp
  where opts = authHeader c
        toParts fp = partFile (T.pack $ takeBaseName fp) fp
        files = map toParts fs
        resp = do (status, body) <- processRequest $ postWith opts tagUrl files
                  let extractedVec = vecOfObjects $ getVec "results" body
                  let code = status ^. statusCode in
                    if code /= 200 then
                      return (Left (code, apiErr code body))
                    else
                      return (Right (V.map objToTagSet extractedVec))
                    where

vecOfObjects :: V.Vector Value -> V.Vector HObj
vecOfObjects = V.map value2Map

-- Given an API Info type and a list of FilePaths, we verify each of the files.
-- If the file has an extension, we decide which Info attribute to use
-- to verify the file. If it has no extension, we choose not to verify. This
-- function maps each FilePath to a tuple of (FilePath, VerificationStatus),
-- where VerificationStatus = Good | Bad | Unknown
verifyFiles :: Info -> [FilePath] -> IO [(FilePath, IO VerificationStatus)]
verifyFiles info fs = do
  let zipped = zip fs (map getFileSize fs)
  return (map (verify info) zipped)
  where verify (Info _ _ _ ib _ _ _ _ vb) (path, ioSize)
          | ext `elem` imageExtensions = (path, fmap imgC ioSize)
          | ext `elem` videoExtensions = (path, fmap vidC ioSize)
          | otherwise = (path, return Unknown)
          where ext = takeExtension path
                vidC = fileCheck vb
                imgC = fileCheck ib

-- Given an API Info type and a list of FilePaths, we verify
-- the length of the list with what the Info type specifies is
-- acceptable batch size for images. This function assumes that
-- the FilePaths all point to images.
verifyImageBatchSize :: Info -> [FilePath] -> Bool
verifyImageBatchSize (Info size _ _ _ _ _ _ _ _) xs = size >= conv
  where conv = fromIntegral (length xs) :: Integer

-- Given an API Info type and a list of FilePaths, we verify
-- the length of the list with what the Info type specifies is
-- acceptable batch size for videos. This function assumes that
-- the FilePaths all point to videos.
verifyVideoBatchSize :: Info -> [FilePath] -> Bool
verifyVideoBatchSize (Info _ _ _ _ size _ _ _ _) xs = size >= conv
  where conv = fromIntegral (length xs) :: Integer
