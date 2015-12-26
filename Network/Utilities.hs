{-# LANGUAGE OverloadedStrings #-}

module Network.Utilities where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map.Lazy              as Map
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                  as T
import           Network.Wreq

type Resp = Response BS.ByteString
type Obj = Map.Map String Value
type JSON = Response Obj
type Errors = (Int, String)

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

-- Gets a value from a map that we know exists
definite :: String -> Obj -> Value
definite k m = fromJust $ Map.lookup k m

-- Convert an Aeson value into a String
value2String :: Value -> String
value2String (String xs) = T.unpack xs
value2String _ = ""

-- Convert an Aeson value into an Int(eger?)
value2Int :: Value -> Integer
value2Int (Number x) = coefficient x
value2Int _ = 0

-- Composes the definite and value2* functions into
-- a single function that gets and converts from a map.
getInt key    = value2Int . definite key
getString key = value2String . definite key

-- custom postWith that overrides default checkStatus functionality.
-- no longer returns an error on non-2** status codes.
postWith' :: (String -> [FormParam] -> IO (Response BS.ByteString) )
postWith' = postWith (set checkStatus (Just $ \_ _ _ -> Nothing) defaults)