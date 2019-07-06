module Tenpureto.GraphTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Text                      ( Text )

import           Tenpureto.Graph

test_filterVertices :: [TestTree]
test_filterVertices =
    [ testCase "keep edges"
        $   s (filterVertices ("b" /=) (path ["a", "b", "c"]))
        @?= path ["a", "c"]
    , testCase "keep disconnected vertices"
        $   s (filterVertices ("b" /=) (vertices ["a", "b", "c"]))
        @?= vertices ["a", "c"]
    ]
  where
    s :: Graph Text -> Graph Text
    s = simplify
