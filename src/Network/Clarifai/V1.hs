{-|

Module      : Network.Clarifai.V1
Description : API Client for the Clarifai V1 API.
Copyright   : (c) Joseph Canero, 2017
License     : MIT
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : portable

Provides functionality for interacting with Clarifai's
Image Tagging API. Allows users to submit images/videos to be recognized and
tagged by Clarifai. Users will need a Clarifai account to make full use of this
client.

-}

module Network.Clarifai.V1
(
  module X
) where

import           Network.Clarifai.V1.API as X
