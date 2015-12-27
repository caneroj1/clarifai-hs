{-# LANGUAGE OverloadedStrings #-}

module Network.Utilities where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Lazy          as Hash
import qualified Data.Map.Lazy              as Map
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                  as T
import           Data.Word
import           Network.Wreq

type Resp = Response BS.ByteString
type Obj = Map.Map String Value
type HObj = Hash.HashMap T.Text Value
type JSON = Response Obj
type Errors = (Int, String)

-- Enum type for verifying FilePaths before sending to the API.
data VerificationStatus = Good | Bad | Unknown

-- Process the results of a request into JSON
-- Returns an IO Tuple of the status and response body.
processRequest :: IO Resp -> IO (Status, Obj)
processRequest response = do  r <- asJSON =<< response :: IO JSON
                              return (r ^. responseStatus, r ^. responseBody)

-- Parses status code errors for api endpoints
apiErr :: Int -> Obj -> String
apiErr code body
  | code == 401 = getString "status_msg" body
  | code == 400 = getString "error"      body

-- Gets a value that we know exists from a map
definite :: String -> Obj -> Value
definite k m = fromJust $ Map.lookup k m

-- Gets a value that we know exists from a hash map
definite' :: String -> HObj -> Value
definite' k m = fromJust $ Hash.lookup (T.pack k) m

-- Convert an Aeson value into a String
value2String :: Value -> String
value2String (String xs) = T.unpack xs
value2String _ = ""

-- Convert an Aeson value into an Integer
value2Int :: Value -> Integer
value2Int (Number x) = coefficient x
value2Int _ = 0

-- Convert an Aeson value into a hash map
value2Map :: Value -> HObj
value2Map (Object o) = o
value2Map _ = Hash.empty

-- Composes the definite and value2* functions into
-- a single function that gets and converts from a map.
getInt key      = value2Int . definite key
getString key   = value2String . definite key
getMap key      = value2Map . definite key
getInt' key     = value2Int . definite' key
getString' key  = value2String . definite' key

-- custom postWith that overrides default checkStatus functionality.
-- no longer returns an error on non-2** status codes.
postWith' :: (String -> [FormParam] -> IO (Response BS.ByteString) )
postWith' = postWith defaults'

defaults' = set checkStatus (Just $ \_ _ _ -> Nothing) defaults

-- List of extensions for videos. If a file has one of these extensions
-- we use the video Info from the API to verify the file.
videoExtensions = [".mov", ".webm", ".ogv", ".ogg", ".wmv", ".mp4",
                   ".m4p", ".qt", ".mpg", ".mp2", ".mpeg", ".mpe"]

-- List of extensions for images. If a file has one of these extensions
-- we use the image Info from the API to verify the file.
imageExtensions = [".png", ".jpeg", ".jpg", ".gif", ".bmp"]

-- Checks that a given file size is within the bounds specified by
-- the (min, max) tuple.
fileCheck :: (Integer, Integer) -> Word64 -> VerificationStatus
fileCheck (minSize, maxSize) size =
  if conv <= maxSize && conv >= minSize
  then Good
  else Bad
  where conv = fromIntegral size :: Integer
