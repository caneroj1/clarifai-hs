{-# LANGUAGE FlexibleInstances #-}

module Network.Clarifai.V2.Types
(
  -- * Base API Types
  APIKey(..)
, ClarifaiT
, Clarifai
) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString            as BS
import           Data.Char
import qualified Data.Text                  as T (unpack)
import           Data.Text.Conversions

-- | newtype wrapper around Clarifai API keys.
newtype APIKey = APIKey {
    unAPIkey :: BS.ByteString
  }

instance FromText APIKey where
  fromText = APIKey . BS.pack . map (fromIntegral . ord) . T.unpack

instance FromText (Maybe APIKey) where
  fromText = Just . fromText

-- | ClarifaiT monad transformer used for running requests.
newtype ClarifaiT m a = ClarifaiT {
    runClarifaiT :: ReaderT APIKey m a
  }

-- | A type synonym for 'ClarifaiT' specialized to run in IO.
type Clarifai a = ClarifaiT IO a

