{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                     hiding ((.=))
import           Data.Aeson
import           Data.String
import           Network.Clarifai.V2.API.Inputs
import           Network.Clarifai.V2.Types.Base
import           Network.Clarifai.V2.Types.Inputs
import           System.Environment

main = do
  k <- fromString <$> apiKey
  print =<< runClarifaiT (addInputs inputs) k
  where apiKey = getEnv "CLARIFAIAPIKEY"

inputs :: [Input]
inputs = [
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
  , input (Url "https://media.timeout.com/images/102600575/image.jpg")
      & allowDuplicate .~ True
      & inputId ?~ "MyImageIdentifier"
      & concepts .~ [
          Concept "Burger" True
        , Concept "Cheese" True
        , Concept "Beef" True
        ]
  ]
