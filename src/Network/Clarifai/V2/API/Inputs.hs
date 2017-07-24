{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai.V2.API.Inputs where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                                 (toJSON)
import           Network.Clarifai.V2.Internal.JsonUtilities
import           Network.Clarifai.V2.Internal.Request
import           Network.Clarifai.V2.Types.Base
import           Network.Clarifai.V2.Types.Inputs

addInputs :: (MonadIO m) => [Input] -> ClarifaiT m ()
addInputs [] = return ()
addInputs is =
  void                     .
  clarifaiPost inputsUrl   .
  toObjectWithKey "inputs" .
  toJSON                   $
  take 128 is
