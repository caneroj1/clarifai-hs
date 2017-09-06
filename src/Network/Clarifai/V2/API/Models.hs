{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Clarifai.V2.API.Models where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid
import           Data.Text                            (Text)
import           Network.Clarifai.V2.Internal.Request
import           Network.Clarifai.V2.Types.Base
import           Network.Clarifai.V2.Types.Inputs     hiding (input)
import           Network.Clarifai.V2.Types.Models

-- | Create a new model with the given ID. A list of 'Concept's can also
-- be provided to initialize the model with a few concepts. An optional
-- 'OutputConfig' can also be specified to customize the model's settings.
createModel :: (MonadIO m) => Text -> [Concept] -> Maybe OutputConfig -> ClarifaiT m (Either ApiError ())
createModel modelID cs optConfig = do
  resp <- clarifaiPost' modelsUrl . toJSON $ CreateModelObject modelID cs optConfig
  return $ void resp

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
      "model" .= toJSON modelData
    ]
    where modelData = baseProps             <>
                      mkOutputInfo concepts <>
                      mkOutputConfig optConfig
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
