{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Clarifai.V2.API.Models
(
  createModel
, createModelWithConfig
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid
import           Data.Text                            (Text)
import           Network.Clarifai.V2.Internal.Request
import           Network.Clarifai.V2.Internal.Types
import           Network.Clarifai.V2.Types.Base
import           Network.Clarifai.V2.Types.Inputs     hiding (input)
import           Network.Clarifai.V2.Types.Models

-- | Create a new model with the given ID. A list of 'Concept's can also
-- be provided to initialize the model with concepts.
createModel :: (MonadIO m) => Text -> [Concept] -> ClarifaiT m (Either ApiError ())
createModel modelID cs = do
  resp <- clarifaiPost' modelsUrl . toJSON $ CreateModelObject modelID cs Nothing
  return $ void resp

-- | Create a new model with the given ID. A list of 'Concept's can also
-- be provided to initialize the model with a few concepts. An
-- 'OutputConfig' can also be specified to customize the model's settings.
createModelWithConfig :: (MonadIO m) => Text -> [Concept] -> OutputConfig -> ClarifaiT m (Either ApiError ())
createModelWithConfig modelID cs config = do
  resp <- clarifaiPost' modelsUrl . toJSON $ CreateModelObject modelID cs (Just config)
  return $ void resp

-- | Add new 'Concept's to the model with the given ID.
updateConcepts :: (MonadIO m) => Text -> [Concept] -> ClarifaiT m (Either ApiError ())
updateConcepts modelID cs = updateConceptsForModels [(modelID, cs)]

-- | Batch add new 'Concept's to many models.
updateConceptsForModels :: (MonadIO m) => [(Text, [Concept])] -> ClarifaiT m (Either ApiError ())
updateConceptsForModels = undefined-- batchChangeConcepts Merge

--------------
-- Internal --
--------------

data CreateModelObject = CreateModelObject {
    modelID   :: Text
  , concepts  :: [Concept]
  , optConfig :: Maybe OutputConfig
  }

instance ToJSON CreateModelObject where
  toJSON CreateModelObject{..} =
    object [
      "model" .= toJSON (mkModelInfo modelID concepts optConfig)
    ]

data UpdateModelsObject = UpdateModelsObject {
    modelIDs       :: Text
  , updateConcepts :: [Concept]
  }

mkModelInfo :: Text -> [Concept] -> Maybe OutputConfig -> [Pair]
mkModelInfo modelID concepts optConfig =
  baseProps             <>
  mkOutputInfo concepts <>
  mkOutputConfig optConfig
  where
    baseProps = ["id" .= String modelID]
    outputInfoProp = maybe []

mkOutputInfo :: [Concept] -> [Pair]
mkOutputInfo [] = []
mkOutputInfo cs = [
    "output_info" .= object [
      "data" .= object [
        "concepts" .= toJSON (toModelConcepts cs)
      ]
    ]
  ]
  where toModelConcepts = map (\c -> object ["id" .= String (conceptId c)])

mkOutputConfig :: Maybe OutputConfig -> [Pair]
mkOutputConfig Nothing   = []
mkOutputConfig (Just oc) = ["output_config" .= toJSON oc]

-- batchChangeInputs :: (MonadIO m) => Action -> [(Text, [Concept])] -> ClarifaiT m (Either ApiError ())
-- batchChangeInputs a cs = do
--   resp <- clarifaiPatch modelsUrl $ toJSON cs
--   return $ void resp
--   where toInputData (iid, cs) = InputData (idToText iid) $ InputDataObject cs
--         req                   = ChangeInputs a $ map toInputData is

-- idToText = T.pack . L.toList
