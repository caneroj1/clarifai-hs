module Network.Clarifai.V2.API
(
  -- * API Requests
  -- predict
) where

-- import           Control.Monad.IO.Class
-- import           Data.ByteString.Lazy      (ByteString)
-- import           Network.Clarifai.V2.Types
-- import           Network.Wreq

-- -- | Using 'Predictions' as input, send a request to the Clarifai API to get predictions for
-- -- a set of pictures.
-- predict :: (MonadIO m) => Predictions -> ClarifaiT m (Response ByteString)
-- predict p = ClarifaiT $ ask >>= \k -> liftIO $ sendClarifaiRequest k predictUrl p
--   where predictUrl = "https://api.clarifai.com/v2/models/aaa03c23b3724a16a56b629203edc62c/outputs"
