{-# LANGUAGE OverloadedStrings #-}

module Network.Clarifai.V2.Internal.JsonUtilities
(
  encodeImage
) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text             (Text)
import           Data.Text.Conversions

encodeImage :: (ToText a) => Text -> a -> Maybe String -> Value
encodeImage k v Nothing = object $ baseImagePairs k v
encodeImage k v (Just iid) = object $
  baseImagePairs k v ++ ["id" .= iid]

baseImagePairs :: (ToText a) => Text -> a -> [Pair]
baseImagePairs k v = [
    "data" .= object [
      "image" .= object [
        k .= toText v
      ]
    ]
  ]
