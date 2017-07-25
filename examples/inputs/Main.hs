{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                     hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Base64.Lazy      as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.String
import           Data.Text.Conversions
import           Network.Clarifai.V2.API.Inputs
import           Network.Clarifai.V2.Types.Base
import           Network.Clarifai.V2.Types.Inputs
import           System.Environment

main = do
  k <- fromString <$> apiKey
  b <- Base64 <$> BL.readFile "./examples/inputs/img/burger.jpg"
  print =<< runClarifaiT (addInputs $ inputs b) k
  where apiKey = getEnv "CLARIFAIAPIKEY"

inputs :: Base64 BL.ByteString -> [Input]
inputs b = [
    input (Url "https://www.omnihotels.com/-/media/images/hotels/homrst/restaurants/homrst-omni-homestead-resort-jeffersons-restaurant-2.jpg")
      & allowDuplicate .~ True
      & metadata .~ [
          "name" .= String "Jeffersons Restaurant"
        ]
  , input (Url "https://media-cdn.tripadvisor.com/media/photo-o/0a/56/44/5a/restaurant.jpg")
      & allowDuplicate .~ True
      & metadata .~ [
          "name" .= String "Costes Restaurant"
        ]
  , input (Image b)
      & allowDuplicate .~ True
      & inputId ?~ "MyImageIdentifier"
      & concepts .~ [
          Concept "Burger" True
        , Concept "Cheese" True
        , Concept "Beef" True
        ]
  ]
