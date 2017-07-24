module Network.Clarifai.V2.Internal.JsonUtilities
(
  toObjectWithKey
) where

import           Data.Aeson
import qualified Data.HashMap.Lazy as M
import           Data.Text         (Text)

toObjectWithKey :: Text -> Value -> Value
toObjectWithKey k = Object . M.singleton k
