{-# LANGUAGE FlexibleInstances #-}

module Network.Clarifai.V2.Utilities
(
  mkAPIKey
) where

import           Data.Text.Conversions
import           Network.Clarifai.V2.Types

-- | Construct an APIKey from text-like types,
-- such as 'String', 'Text', or 'ByteString'
mkAPIKey :: (ToText a) => a -> Maybe APIKey
mkAPIKey = convertText
