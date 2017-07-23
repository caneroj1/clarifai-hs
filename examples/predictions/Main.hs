module Main where

import           Network.Clarifai.V2
import           System.Environment

main = undefined

-- main = do
--   Just k <- mkAPIKey <$> apiKey
--   print =<< runClarifaiT (predict predictions) k
--   where apiKey = getEnv "CLARIFAIAPIKEY"

-- predictions :: Predictions
-- predictions = Predictions [
--     Url "https://www.omnihotels.com/-/media/images/hotels/homrst/restaurants/homrst-omni-homestead-resort-jeffersons-restaurant-2.jpg"
--   , Url "https://media-cdn.tripadvisor.com/media/photo-o/0a/56/44/5a/restaurant.jpg"
--   , UrlId "https://media.timeout.com/images/102600575/image.jpg" (Id "MyIdentifier")
--   ]
