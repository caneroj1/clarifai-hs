{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Clarifai.V2.API.Inputs
(
  -- * Input API
  -- ** Creating Inputs
  addInputs
  -- ** Retrieving Inputs
, getInput
, getInputs
  -- ** Modifying Inputs
  -- *** Updating Concepts
, updateConcepts
, updateConceptsForInputs
  -- *** Deleting Concepts
, deleteConcepts
, deleteConceptsForInputs
  -- ** Deleting Inputs
, deleteInput
, deleteInputs
, deleteAll
) where

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
import           Network.Clarifai.V2.Internal.Types
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

-- | Get all inputs currently added to the Clarifai API.
getInputs :: (MonadIO m) => ClarifaiT m (Either ApiError [SavedInput])
getInputs = do
  resp <- clarifaiGet inputsUrl
  return $ fmap (fromJust . fmap inputs . decode') resp

-- | Retrieve a specific input by its id. If the input does not exist,
-- a 'NoAccess' 'ApiError' is returned.
getInput :: (MonadIO m) => L.NonEmpty Char -> ClarifaiT m (Either ApiError SavedInput)
getInput iid = do
    resp <- clarifaiGet (inputsUrl ++ "/" ++ L.toList iid)
    return $ fmap (fromJust . fmap input . decode') resp

-- | Add new concepts to an uploaded input.
updateConcepts :: (MonadIO m) => L.NonEmpty Char -> [Concept] -> ClarifaiT m (Either ApiError ())
updateConcepts iid cs = updateConceptsForInputs [(iid, cs)]

-- | Batch add new concepts to many inputs.
updateConceptsForInputs :: (MonadIO m) => [(L.NonEmpty Char, [Concept])] -> ClarifaiT m (Either ApiError ())
updateConceptsForInputs = batchChangeInputs Merge

-- | Remove concepts from an uploaded input.
deleteConcepts :: (MonadIO m) => L.NonEmpty Char -> [Concept] -> ClarifaiT m (Either ApiError ())
deleteConcepts iid cs = deleteConceptsForInputs [(iid, cs)]

-- | Batch remove concepts from many inputs.
deleteConceptsForInputs :: (MonadIO m) => [(L.NonEmpty Char, [Concept])] -> ClarifaiT m (Either ApiError ())
deleteConceptsForInputs = batchChangeInputs Remove

-- | Delete an uploaded input.
deleteInput :: (MonadIO m) => L.NonEmpty Char -> ClarifaiT m (Either ApiError ())
deleteInput iid = do
  resp <- clarifaiDelete (inputsUrl ++ "/" ++ L.toList iid) Nothing
  return $ void resp

-- | Deletes each uploaded input. The delete happens asynchronously.
deleteInputs :: (MonadIO m) => [L.NonEmpty Char] -> ClarifaiT m (Either ApiError ())
deleteInputs [] = return $ Right ()
deleteInputs is = do
  resp <- clarifaiDelete inputsUrl $ Just deleteBody
  return $ void resp
  where deleteBody = toObjectWithKey "ids" . toJSON $ map idToText is

-- | Deletes all uploaded inputs. The delete happens asynchronously.
deleteAll :: (MonadIO m) => ClarifaiT m (Either ApiError ())
deleteAll = do
  resp <- clarifaiDelete inputsUrl $ Just deleteBody
  return $ void resp
  where deleteBody = toObjectWithKey "delete_all" $ Bool True

--------------
-- Internal --
--------------

batchChangeInputs :: (MonadIO m) => Action -> [(L.NonEmpty Char, [Concept])] -> ClarifaiT m (Either ApiError ())
batchChangeInputs a is = do
  resp <- clarifaiPatch inputsUrl $ toJSON is
  return $ void resp
  where toInputData (iid, cs) = InputData (idToText iid) $ InputDataObject cs
        req                   = ChangeInputs a $ map toInputData is

idToText = T.pack . L.toList

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

instance ToJSON Action where
  toJSON Merge  = String "merge"
  toJSON Remove = String "remove"

-- Data types representing input JSON
-- to Clarifai's API for updating/deleting concepts
-- on inputs.
newtype InputDataObject = InputDataObject {
    inputConcepts :: [Concept]
  }

instance ToJSON InputDataObject where
  toJSON InputDataObject{..} = object ["concepts" .= inputConcepts]

data InputData = InputData {
    inputId   :: T.Text
  , inputData :: InputDataObject
  }

instance ToJSON InputData where
  toJSON InputData{..} = object [
      "id" .= inputId
    , "data" .= inputData
    ]

data ChangeInputs = ChangeInputs {
    changeAction    :: Action
  , changeInputData :: [InputData]
  }

instance ToJSON ChangeInputs where
  toJSON ChangeInputs{..} = object [
      "action" .= changeAction
    , "inputs" .= changeInputData
    ]
