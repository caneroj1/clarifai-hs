{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Clarifai.V2.Types
(
  -- * Base API Types
  APIKey(..)
, ClarifaiT
, runClarifaiT
, Clarifai
  -- * API Inputs
, Predictions(..)
, Image(..)
, Identifier(..)
  -- * API Requests
, predict
) where

import           Control.Lens                               hiding ((.=))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as BL
import           Data.Char
import           Data.Monoid
import qualified Data.Text                                  as T (unpack)
import           Data.Text.Conversions
import           GHC.Generics
import           Network.Clarifai.V2.Internal.JsonUtilities
import           Network.Wreq

import           Debug.Trace

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
    unClarifaiT :: ReaderT APIKey m a
  }

-- | Run the 'ClarifaiT' monad with an APIKey.
runClarifaiT :: ClarifaiT m a -> APIKey -> m a
runClarifaiT c apiKey = unClarifaiT c `runReaderT` apiKey

-- | A type synonym for 'ClarifaiT' specialized to run in IO.
type Clarifai a = ClarifaiT IO a

-- class Url a where
--   toUrl :: a -> String

-- class (Url a, ToJSON a, FromJSON (ClarifaiResponse a)) => ClarifaiRequest a where
--   type ClarifaiResponse a
--   send :: (MonadIO m) => a -> ClarifaiT m (Either String (ClarifaiResponse a))
--   send a = ClarifaiT $ ask >>= \k -> liftIO $ sendClarifaiPOST k (toUrl a) (toJSON a)
--                            >>= eitherDecode'

-- | Newtype wrapper around strings that can be used as identifiers for
-- objects uploaded to Clarifai's API.
newtype Identifier = Id String

-- | The 'Image' type represents an image that will be presented to Clarifai
-- in order to receive predictions from a model.
data Image = -- ^ Url of an image that will be passed as input
             Url String
             -- ^ A Base64-encoded version of an image
           | Image (Base64 BS.ByteString)
             -- ^ Url of an image that will be passed as input, along with an identifier
           | UrlId String Identifier
             -- ^ A Base64-encoded version of an image, along with an identifier
           | ImageId (Base64 BS.ByteString) Identifier

instance ToJSON Image where
  toJSON (Url s)            = encodeImage "url" s Nothing
  toJSON (Image b)          = encodeImage "base64" b Nothing
  toJSON (UrlId s (Id i))   = encodeImage "url" s (Just i)
  toJSON (ImageId b (Id i)) = encodeImage "base64" b (Just i)

-- | 'Predictions' represents input values to be passed to Clarifai's predict API.
-- It holds onto numerous images that will be used to get predictions. Clarifai supports
-- up to 128 images in a given predict API call. When preparing to send a request to get predictions,
-- we will choose the first 128 images from the input list.
newtype Predictions = Predictions {
    inputs :: [Image]
  } deriving (Generic)

instance ToJSON Predictions where

-- instance ClarifaiRequest Predictions where

sendClarifaiPOST :: APIKey -> String -> Value -> IO (Response BL.ByteString)
sendClarifaiPOST apiKey = postWith apiKeyOptions
  where apiKeyOptions = defaults & header "Authorization" .~ ["Key " <> unAPIkey apiKey]

-- | Using 'Predictions' as input, send a request to the Clarifai API to get predictions for
-- a set of pictures.
predict :: (MonadIO m) => Predictions -> ClarifaiT m (Response BL.ByteString)
predict p = ClarifaiT $ ask >>= \k -> liftIO $ sendClarifaiPOST k predictUrl (trace (show $ encode p) (toJSON p))
  where predictUrl = "https://api.clarifai.com/v2/models/aaa03c23b3724a16a56b629203edc62c/outputs"
