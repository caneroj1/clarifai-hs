{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Clarifai.V2.Types.Base
(
  -- * Base API Types
  APIKey(..)
  -- ** ClarifaiT monad transformer
, ClarifaiT(..)
, ClarifaiIO
, runClarifaiT
) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString            as BS
import           Data.Char
import           Data.String

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
