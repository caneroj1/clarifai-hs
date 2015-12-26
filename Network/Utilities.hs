{-# LANGUAGE OverloadedStrings #-}

module Network.Utilities where

import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text                  as T
import           Network.Wreq

type Resp = Response BS.ByteString

-- Process the results of a request
-- Returns an IO Tuple of the status and response body.
processRequest :: IO Resp -> IO (Status, BS.ByteString)
processRequest response = do  r <- response
                              return (r ^. responseStatus, r ^. responseBody)

authErr :: Int -> BS.ByteString -> T.Text
authErr code body
  | code == 401 = body ^. key "status_msg" . _String
  | code == 400 = body ^. key "error"      . _String


-- custom postWith that overrides default checkStatus functionality.
-- no longer returns an error on non-2** status codes.
postWith' :: (String -> [FormParam] -> IO (Response BS.ByteString) )
postWith' = postWith (set checkStatus (Just $ \_ _ _ -> Nothing) defaults)
