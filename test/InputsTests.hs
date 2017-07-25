{-# LANGUAGE OverloadedStrings #-}

module InputsTests
(
  inputsTests
) where

import           Control.Lens                     hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy             as BS
import           Data.HashMap.Lazy                as M
import           Data.Text                        (Text)
import qualified Data.Vector                      as V
import           Helpers
import           Network.Clarifai.V2.Types.Inputs
import           Test.HUnit.Base

inputsTests :: [Test]
inputsTests = [
    -- serializing inputs
    TestCase basicInput
  , TestCase inputWithConcepts
  , TestCase inputWithMetadata
    -- deserializing inputs
  , TestCase basicSavedInput
  , TestCase savedInputWithConcepts
  , TestCase savedInputWithMetadata
  ]

basicInput :: Assertion
basicInput = expected @=? toJSON (input (Url "test.png"))
  where
    expected =
      Object $
        M.fromList [
          ("data", Object $
            M.fromList [
              ("image", Object $
                M.fromList [
                  ("allow_duplicate_url", Bool False)
                , ("url", String "test.png")
                ])
            , ("concepts", Array V.empty)
            ])
        ]

inputWithConcepts :: Assertion
inputWithConcepts = expected @=? toJSON (input (Url "test.png") & concepts .~ testConcepts)
  where
    testConcepts = [Concept "c1" True, Concept "c2" False]
    expected =
      Object $
        M.fromList [
          ("data", Object $
            M.fromList [
              ("image", Object $
                M.fromList [
                  ("allow_duplicate_url", Bool False)
                , ("url", String "test.png")
                ])
            , ("concepts", Array      .
                           V.fromList $ [
                              Object $ M.fromList [("id", "c1"), ("value", Bool True)]
                            , Object $ M.fromList [("id", "c2"), ("value", Bool False)]
                            ])
            ])
        ]

inputWithMetadata :: Assertion
inputWithMetadata = expected @=? toJSON (input (Url "test.png") & metadata .~ testMeta)
  where
    testMeta = ["extra" .= Object extraMeta, "name" .= String "My Test Image"]
    extraMeta = M.fromList [
          ("size", Number 1024)
        , ("type", String "png")
        ]
    expected =
      Object $
        M.fromList [
          ("data", Object $
            M.fromList [
              ("image", Object $
                M.fromList [
                  ("allow_duplicate_url", Bool False)
                , ("url", String "test.png")
                ])
            , ("concepts", Array V.empty)
            , ("metadata", Object $
                           M.fromList [
                              ("extra", Object extraMeta)
                            , ("name", String "My Test Image")
                           ])
            ])
        ]

basicSavedInput :: Assertion
basicSavedInput = do
  basicInput <- inputJSON
  expected @=? basicInput
  where
    inputJSON = eitherDecode' <$> BS.readFile "./test/data/basic_input.json"
    expected =
      Right $
        SavedInput {
            _savedInputId = "TestIdentifier"
          , _savedInputCreatedAt = toUTC "2017-07-25T00:48:46.184625Z"
          , _savedInputModifiedAt = toUTC "2017-07-25T00:48:46.672809Z"
          , _savedInputStatus = SavedInputStatus {
              _statusCode = 30000
            , _statusDescription = "Download complete"
            }
          , _savedInputData = SavedInputData {
              _savedConcepts = Nothing
            , _savedMetadata = emptyObject
            , _savedImage = Url "http://my.image.com"
            }
          }

savedInputWithConcepts :: Assertion
savedInputWithConcepts = do
  conceptsInput <- inputJSON
  expected @=? conceptsInput
  where
    inputJSON = eitherDecode' <$> BS.readFile "./test/data/concepts_input.json"
    expected =
      Right $
        SavedInput {
            _savedInputId = "TestIdentifier"
          , _savedInputCreatedAt = toUTC "2017-07-25T00:48:46.184625Z"
          , _savedInputModifiedAt = toUTC "2017-07-25T00:48:46.672809Z"
          , _savedInputStatus = SavedInputStatus {
              _statusCode = 30000
            , _statusDescription = "Download complete"
            }
          , _savedInputData = SavedInputData {
              _savedConcepts = Just [
                  Concept "Burger" True
                , Concept "Cheese" True
                , Concept "Beef" True
                ]
            , _savedMetadata = emptyObject
            , _savedImage = Url "http://my.image.com"
            }
          }

savedInputWithMetadata :: Assertion
savedInputWithMetadata = do
  metadataInput <- inputJSON
  expected @=? metadataInput
  where
    inputJSON = eitherDecode' <$> BS.readFile "./test/data/metadata_input.json"
    expected =
      Right $
        SavedInput {
            _savedInputId = "TestIdentifier"
          , _savedInputCreatedAt = toUTC "2017-07-25T00:48:46.184625Z"
          , _savedInputModifiedAt = toUTC "2017-07-25T00:48:46.672809Z"
          , _savedInputStatus = SavedInputStatus {
              _statusCode = 30000
            , _statusDescription = "Download complete"
            }
          , _savedInputData = SavedInputData {
              _savedConcepts = Nothing
            , _savedMetadata = object ["name" .= String "Costes Restaurant"]
            , _savedImage = Url "http://my.image.com"
            }
          }
