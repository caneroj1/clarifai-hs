module Main where

import           Control.Monad
import           InputsTests
import           Test.HUnit.Base
import           Test.HUnit.Text

main = void . runTestTT . TestList $
     inputsTests
