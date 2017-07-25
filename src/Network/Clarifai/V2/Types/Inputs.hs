{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Clarifai.V2.Types.Inputs
(
  -- * Types
  Top
, Bot
, Left
, Right
, Crop(..)
, Concept(..)
, Image(..)
  -- ** Input
, Input(..)
  -- *** Input Lenses
, crop
, metadata
, inputId
, concepts
, allowDuplicate
, image
  -- *** Constructing Inputs
, input
  -- ** Saved Input
, SavedInput(..)
  -- *** Saved Input Lenses
, savedInputId
, savedInputCreatedAt
, savedInputModifiedAt
, savedInputStatus
, savedInputData
, SavedInputStatus(..)
  -- *** Saved Input Status Lenses
, statusCode
, statusDescription
, SavedInputData(..)
  -- *** Saved Input Data Lenses
, savedConcepts
, savedMetadata
, savedImage
) where

import           Control.Lens          hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy  as BS
import           Data.Default.Class
import qualified Data.HashMap.Lazy     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.Text             (Text)
import           Data.Text.Conversions
import           Data.Time.Clock
import qualified Data.Vector           as V

type Top = Double
type Bot = Double
type Left = Double
type Right = Double

-- | A 'Crop' specifies setting by which an input image will be cropped.
-- The 'Top' and 'Bot' fields specify the percentage by which the image will be cropped
-- relative to the top, and the 'Left' and 'Right' fields specify the percentage
-- by which the image will be cropped relative to the left.
data Crop = Crop Top Left Bot Right deriving (Show, Eq)

-- | A 'Concept' is a data type that encodes an id and value pair
-- of something contained in an image.
data Concept = Concept {
    conceptId    :: Text -- ^ Identifier for the concept
  , conceptValue :: Bool -- ^ Value of the concept
  } deriving (Show, Eq)

instance FromJSON Concept where
  parseJSON (Object o) = Concept <$>
                           o .: "id" <*>
                           (toEnum <$> o .: "value")
  parseJSON v          = typeMismatch "Concept" v

-- | An 'Image' represents a url to an image or a base64 encoded blob that can
-- be passed to Clarifai for processing.
data Image = -- ^ Url of an image that will be passed as input
             Url String
             -- ^ A Base64-encoded version of an image
           | Image (Base64 BS.ByteString)
  deriving (Show, Eq)

instance FromJSON Image where
  parseJSON (Object o)
    | M.member "url" o    = Url <$> o .: "url"
    | M.member "base64" o = Image .
                            fromJust (error "Could not decode base64 for Image") .
                            getTextFromJSON <$> o .: "base64"
    | otherwise         = error "Could not parse JSON for Image"
    where getTextFromJSON :: Text -> Maybe (Base64 BS.ByteString)
          getTextFromJSON = fromText

-- | An 'Input' represents a collection of parameters and metadata about
-- an image that will be passed to Clarifai.
data Input = Input {
    _crop           :: Maybe Crop -- ^ Optional 'Crop' settings for the input.
  , _metadata       :: [Pair]     -- ^ Custom metadata for the image.
  , _inputId        :: Maybe Text -- ^ Optional 'Text' identifier for this image.
  , _concepts       :: [Concept]  -- ^ A list of 'Concept's that are contained in the image.
  , _allowDuplicate :: Bool       -- ^ 'Bool' parameter indicating whether or not Clarifai should allow duplicate inputs.
                                  -- If an input identifier is specified, that must still be unique.
  , _image          :: Image      -- ^ The actual 'Image' parameter for this 'Input'.
  } deriving (Show, Eq)
makeLenses ''Input

instance ToJSON Input where
  toJSON i =
    object $ (
      "data" .= object ([buildImage, buildConcepts] ++ buildMetadata)
    ) : buildId
    where
      -- various helper functions for building up the JSON payload for a single Input.
      buildImage = ("image", object imgData)
        where imgData = [("allow_duplicate_url", Bool $ _allowDuplicate i)] ++
                        imgFields (_image i)                                ++
                        maybe [] cropFields (_crop i)
      buildConcepts = ("concepts", Array . V.fromList . map (object . conceptFields) $ _concepts i)
      buildMetadata
        | null $ _metadata i = []
        | otherwise          = [("metadata", object $ _metadata i)]
      buildId = maybe [] (\i -> [("id", String i)]) $ _inputId i

      -- turn an image into a list of json attributes
      imgFields (Url s)   = [("url", String $ toText s)]
      imgFields (Image b) = [("base64", String $ toText b)]

      -- turn a concept into a list of json attributes
      conceptFields c = [("id", String $ conceptId c), ("value", Bool $ conceptValue c)]

      -- turn a crop into a list of json attributes
      cropFields (Crop t l b r) = [
          ("crop", Array . V.fromList $ map (Number . fromFloatDigits) [t, l, b, r])
        ]

-- | Smart constructor for an 'Input'. Accepts an 'Image' and creates an 'Input' out of it
-- with sensible defaults.
input :: Image -> Input
input img = Input {
    _crop = Nothing
  , _metadata = []
  , _inputId = Nothing
  , _concepts = []
  , _allowDuplicate = False
  , _image = img
  }

-- | A 'SavedInputStatus' represents the current status of an input in Clarifai's backend.
data SavedInputStatus = SavedInputStatus {
    _statusCode        :: Int
  , _statusDescription :: Text
  } deriving (Show, Eq)
makeLenses ''SavedInputStatus

instance FromJSON SavedInputStatus where
  parseJSON (Object o) = SavedInputStatus <$>
                           o .: "code" <*>
                           o .: "description"
  parseJSON v          = typeMismatch "SavedInputStatus" v

-- | A 'SavedInputData' contains all of the data that has been saved about an image in Clarifai's backend.
data SavedInputData = SavedInputData {
    _savedConcepts :: Maybe [Concept]
  , _savedMetadata :: Value
  , _savedImage    :: Image
  } deriving (Show, Eq)
makeLenses ''SavedInputData

instance FromJSON SavedInputData where
  parseJSON (Object o) = SavedInputData <$>
                           o .:? "concepts" <*>
                           o .: "metadata"  <*>
                           o .: "image"
  parseJSON v = typeMismatch "SavedInputData" v

-- | A 'SavedInput' represents an 'Input' value that has been persisted to Clarifai's backend.
data SavedInput = SavedInput {
    _savedInputId         :: Text
  , _savedInputCreatedAt  :: UTCTime
  , _savedInputModifiedAt :: UTCTime
  , _savedInputStatus     :: SavedInputStatus
  , _savedInputData       :: SavedInputData
  } deriving (Show, Eq)
makeLenses ''SavedInput

instance FromJSON SavedInput where
  parseJSON (Object o) = SavedInput <$>
                           o .: "id"          <*>
                           o .: "created_at"  <*>
                           o .: "modified_at" <*>
                           o .: "status"      <*>
                           o .: "data"
  parseJSON v = typeMismatch "SavedInput" v
