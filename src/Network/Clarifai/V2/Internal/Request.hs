{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai.V2.Internal.Request where

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                      as T
import           GHC.Generics
import           Network.Clarifai.V2.Types.Base
import           Network.HTTP.Client            hiding (responseBody,
                                                 responseStatus)
import           Network.Wreq
import           Network.Wreq.Lens

-- Urls
inputsUrl = "https://api.clarifai.com/v2/inputs"

type ApiResponse = Either ApiError BL.ByteString

clarifaiPost :: (MonadIO m) => String -> Value -> ClarifaiT m (Response BL.ByteString)
clarifaiPost url d = ClarifaiT $ ask >>= \k -> liftIO $ postWith (apiKeyOptions k) url d
  where apiKeyOptions k = defaults & header "Authorization" .~ ["Key " <> unAPIkey k]

clarifaiGet :: (MonadIO m) => String -> ClarifaiT m ApiResponse
clarifaiGet url = ClarifaiT $ do
  key <- ask
  liftIO $
    fmap (Right . body) (getWith (apiKeyOptions key) url) `catch` (return . Left . catchHttpException)
  where apiKeyOptions k = defaults & header "Authorization" .~ ["Key " <> unAPIkey k]

clarifaiPatch :: (MonadIO m) => String -> Value -> ClarifaiT m ApiResponse
clarifaiPatch url d = ClarifaiT $ do
  key <- ask
  liftIO $
    fmap (Right . body) (patchWith (apiKeyOptions key) url d) `catch` (return . Left . catchHttpException)
  where apiKeyOptions k = defaults & header "Authorization" .~ ["Key " <> unAPIkey k]

patchWith = customPayloadMethodWith "PATCH"

catchHttpException :: HttpException -> ApiError
catchHttpException (InvalidUrlException url _) = BadUrl $ T.pack url
catchHttpException (HttpExceptionRequest _ content) = handleExceptionContent content

handleExceptionContent :: HttpExceptionContent -> ApiError
handleExceptionContent (StatusCodeException r body) = handleStatusException r (BL.fromStrict body)
handleExceptionContent _ = Unknown

newtype ApiStatus = ApiStatus {
    status :: ApiMessage
  } deriving (Generic)

instance FromJSON ApiStatus where

handleStatusException :: Response () -> BL.ByteString -> ApiError
handleStatusException r body =
  case r ^. responseStatus . statusCode of
    400 -> parseAsError BadRequest
    401 -> NoAccess
    403 -> NoAccess
    404 -> parseAsError NotFound
    500 -> Internal
    _   -> Unknown
  where parseAsError err = maybe Unknown (err . status) $ decode' body

body :: Response BL.ByteString -> BL.ByteString
body r = r ^. responseBody
