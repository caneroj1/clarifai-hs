module Helpers where

import           Data.Text
import           Data.Time.Clock
import           Data.Time.Format

toUTC :: Text -> UTCTime
toUTC = utcParser
  where
    utcParser = parseTimeOrError False
                defaultTimeLocale
                "%FT%X%QZ" .
                unpack
