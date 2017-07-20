module Network.Clarifai.V2.Models where

-- | Data type for the various models available
-- through Clarifai.
data Model = -- ^ Recognize clothing, accessories, and other fashion-related items
             Apparel
             -- ^ Identify celebrities that closely resemble detected faces
           | Celebrity
             -- ^ Identify the dominant colors present in your media in hex or W3C form
           | Color
             -- ^ Predict the age, gender, and cultural appearance of detected faces
           | Demographics
             -- ^ Detect the presence and location of human faces with a bounding box
           | FaceDetection
             -- ^ Returns overall focus score and identifies in-focus regions within an image
           | Focus
             -- ^ Recognize food items and dishes, down to the ingredient level
           | Food
             -- ^ Clarifai's most comprehensive model with concepts, objects, scenes, and more
           | General
             -- ^ Computes numerical embedding vectors using the 'General' model
           | GeneralEmbedding
             -- ^ Detect and identify brand logos within images
           | Logo
             -- ^ Identify different levels of nudity in visual content
           | NSFW
             -- ^ Understand travel and hospitality-related concepts
           | Travel
             -- ^ Understand wedding-related concepts like bride, groom, flowers, and more
           | Wedding
           deriving (Show, Eq)

class IsModel a where
  modelId :: a -> String

instance IsModel Model where
  modelId Apparel          = "e0be3b9d6a454f0493ac3a30784001ff"
  modelId Celebrity        = "e466caa0619f444ab97497640cefc4dc"
  modelId Color            = "eeed0b6733a644cea07cf4c60f87ebb7"
  modelId Demographics     = "c0c0ac362b03416da06ab3fa36fb58e3"
  modelId FaceDetection    = "a403429f2ddf4b49b307e318f00e528b"
  modelId Focus            = "c2cf7cecd8a6427da375b9f35fcd2381"
  modelId Food             = "bd367be194cf45149e75f01d59f77ba7"
  modelId General          = "aaa03c23b3724a16a56b629203edc62c"
  modelId GeneralEmbedding = "bbb5f41425b8468d9b7a554ff10f8581"
  modelId Logo             = "c443119bf2ed4da98487520d01a0b1e3"
  modelId NSFW             = "e9576d86d2004ed1a38ba0cf39ecb4b1"
  modelId Travel           = "eee28c313d69466f836ab83287a54ed9"
  modelId Wedding          = "c386b7a870114f4a87477c0824499348"
