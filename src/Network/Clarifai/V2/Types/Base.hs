{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Clarifai.V2.Types.Base
(
  -- * Base API Types
  APIKey(..)
  -- ** ClarifaiT monad transformer
, ClarifaiT(..)
, ClarifaiIO
, runClarifaiT
  -- ** Error Handling
, ApiMessage(..)
, ApiError(..)
) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString            as BS
import           Data.Char
import           Data.String
import qualified Data.Text                  as T
import           GHC.Generics

-- | newtype wrapper around Clarifai API keys.
newtype APIKey = APIKey {
    unAPIkey :: BS.ByteString
  }

instance IsString APIKey where
  fromString = APIKey . BS.pack . map (fromIntegral . ord)

-- | ClarifaiT monad transformer used for running requests.
newtype ClarifaiT m a = ClarifaiT {
    unClarifaiT :: ReaderT APIKey m a
  } deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadIO (ClarifaiT m) where
  liftIO = ClarifaiT . liftIO

-- | Run the 'ClarifaiT' monad with an APIKey.
runClarifaiT :: ClarifaiT m a -> APIKey -> m a
runClarifaiT c apiKey = unClarifaiT c `runReaderT` apiKey

-- | A type synonym for 'ClarifaiT' specialized to run in IO.
type ClarifaiIO a = ClarifaiT IO a

-- | An 'ApiMessage' represents some status code and message
-- returned from the Clarifai API regarding a request.
data ApiMessage = ApiMessage {
    code        :: Int
  , description :: T.Text
  , details     :: T.Text
  } deriving (Generic, Show, Eq)

instance FromJSON ApiMessage where

-- | An 'ApiError' encapsulates various errors that can go wrong
-- when submitting requests to Clarifai's API.
data ApiError = -- ^ Indicates that the request was forbidden or unauthorized.
                NoAccess
                -- ^ Indicates that a given resource does not exist.
              | NotFound ApiMessage
                -- ^ Indicates that request, in some way, was malformed.
              | BadRequest ApiMessage
                -- ^ Indicates that there was an internal error with the server.
              | Internal
                -- ^ Indicates that a url, in some way, was malformed. The value is the url.
              | BadUrl T.Text
                -- ^ Indicates that something unaccounted for went wrong.
              | Unknown
  deriving (Show, Eq)
