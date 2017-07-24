{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai.V2.Internal.Request where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Monoid
import           Network.Clarifai.V2.Types.Base
import           Network.Wreq

-- Urls
inputsUrl = "https://api.clarifai.com/v2/inputs"

clarifaiPost :: (MonadIO m) => String -> Value -> ClarifaiT m (Response BL.ByteString)
clarifaiPost url d = ClarifaiT $ ask >>= \k -> liftIO $ postWith (apiKeyOptions k) url d
  where apiKeyOptions k = defaults & header "Authorization" .~ ["Key " <> unAPIkey k]
