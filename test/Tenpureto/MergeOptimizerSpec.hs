module Tenpureto.MergeOptimizerSpec
    ( spec
    ) where

import           Data.Functor.Identity
import           Data.Text                      ( Text )
import           Test.Syd

import           Tenpureto.Graph
import           Tenpureto.MergeOptimizer
import qualified Tenpureto.OrderedMap          as OrderedMap

spec :: Spec
spec = describe "mergeGraph" $ do
    it "not merge parents into children"
        $ let graph = edge (v 1 "1") (v 2 "2")
          in  runIdentity (mergeGraph mergeCommits graph)
                  `shouldBe` Just (v 2 "2")
    it "merge siblings"
        $ let graph = overlay (vertex $ v 1 "1") (vertex $ v 2 "2")
          in  runIdentity (mergeGraph mergeCommits graph)
                  `shouldBe` Just (v 3 "1+2")

  where
    mergeCommits c1 c2 _ = return (c1 + c2)
    v :: Int -> Text -> MergedBranchInformation Int
    v x n = MergedBranchInformation
        { mergedBranchMeta       = x
        , mergedBranchDescriptor = MergedBranchDescriptor
                                       { mergedBranchName = n
                                       , mergedVariables  = OrderedMap.empty
                                       , mergedExcludes   = mempty
                                       , mergedConflicts  = mempty
                                       , mergedFeatures   = mempty
                                       }
        }
