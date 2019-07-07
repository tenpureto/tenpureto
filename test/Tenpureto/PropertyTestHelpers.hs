module Tenpureto.PropertyTestHelpers where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog.Classes

lawTestTree :: Laws -> TestTree
lawTestTree laws = testGroup (lawsTypeClass laws)
    $ fmap (uncurry testProperty) (lawsProperties laws)
