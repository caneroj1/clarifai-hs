{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Clarifai.V2.Types.Models
(
  -- * Types
  OutputConfig
  -- ** OutputConfig Lenses
, conceptsMutuallyExclusive
, closedEnvironment
) where

import           Control.Lens hiding ((.=))
import           Data.Aeson

-- | An 'OutputConfig' holds onto configuration
-- settings used to control how the model outputs
-- its results.
data OutputConfig = OutputConfig {
    _conceptsMutuallyExclusive :: Bool
  , _closedEnvironment         :: Bool
  } deriving (Show, Eq)
makeLenses ''OutputConfig

instance ToJSON OutputConfig where
  toJSON OutputConfig{..} =
    object [
      "concepts_mutually_exclusive" .= _conceptsMutuallyExclusive
    , "closed_environment" .= _closedEnvironment
    ]
