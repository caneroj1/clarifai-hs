{-# LANGUAGE OverloadedStrings #-}

module InputsTests
(
  inputsTests
) where

import           Control.Lens
import           Data.Aeson
import           Data.HashMap.Lazy                as M
import           Data.Text                        (Text)
import qualified Data.Vector                      as V
import           Network.Clarifai.V2.Types.Inputs
import           Test.HUnit.Base

inputsTests :: [Test]
inputsTests = [
    TestCase basicInput
  , TestCase inputWithConcepts
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
