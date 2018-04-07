module Network.Clarifai.V2.Internal.Types where

-- When updating or deleting concepts, there is a specific
-- "action" value that should be sent to the API.
data Action = Merge
            | Remove
  deriving (Eq, Show)