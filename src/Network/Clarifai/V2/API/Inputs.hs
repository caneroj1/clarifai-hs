{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai.V2.API.Inputs
(
  -- * Input API
  -- ** Creating Inputs
  addInputs
  -- ** Retrieving Inputs
, getInputs
, getInput
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.List.NonEmpty                         as L
import           Data.Maybe
import qualified Data.Text                                  as T
import           GHC.Generics
import           Network.Clarifai.V2.Internal.JsonUtilities
import           Network.Clarifai.V2.Internal.Request
import           Network.Clarifai.V2.Types.Base
import           Network.Clarifai.V2.Types.Inputs           hiding (input)
import           Network.Wreq

-- | Add inputs to the Clarifai API. Clarifai limits the number of uploads
-- in a given request to 128. This will function respects that
-- limit and only uploads the first 128 inputs.
addInputs :: (MonadIO m) => [Input] -> ClarifaiT m ()
addInputs [] = return ()
addInputs is =
  void                     .
  clarifaiPost inputsUrl   .
  toObjectWithKey "inputs" .
  toJSON                   $
  take 128 is

-- Clarifai's API responds with nested objects
-- when returning inputs
newtype Inputs = Inputs {
    inputs :: [SavedInput]
  } deriving (Generic)

instance FromJSON Inputs where

newtype SingleInput = SingleInput {
    input :: SavedInput
  } deriving (Generic)

instance FromJSON SingleInput where

-- | Get all inputs currently added to the Clarifai API.
getInputs :: (MonadIO m) => ClarifaiT m (Either ApiError [SavedInput])
getInputs = do
  resp <- clarifaiGet inputsUrl
  return $ fmap (fromJust . fmap inputs . decode') resp

-- | Retrieve a specific input by its id.
getInput :: (MonadIO m) => L.NonEmpty Char -> ClarifaiT m (Either ApiError SavedInput)
getInput inputId = do
    resp <- clarifaiGet (inputsUrl ++ "/" ++ L.toList inputId)
    return $ fmap (fromJust . fmap input . decode') resp
