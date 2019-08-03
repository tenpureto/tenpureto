module Tenpureto.FeatureMergerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Algebra.Graph.ToGraph

import           Tenpureto.Graph
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                )
import           Tenpureto.TemplateLoader.Internal
                                                ( FeatureStability(..)
                                                , TemplateYaml(..)
                                                , TemplateYamlFeature(..)
                                                )
import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.FeatureMerger


v :: Text -> TemplateBranchInformation
v c = TemplateBranchInformation
    { branchName   = c
    , branchCommit = Committish ""
    , templateYaml = TemplateYaml
                         { yamlVariables = Map.singleton c c
                         , yamlExcludes  = Set.singleton c
                         , yamlConflicts = Set.singleton c
                         , yamlFeatures  = Set.singleton $ TemplateYamlFeature
                                               { yamlFeatureName        = c
                                               , yamlFeatureDescription = Nothing
                                               , yamlFeatureHidden      = False
                                               , yamlFeatureStability   = Stable
                                               }
                         }
    }

test_runMergeGraphPure :: [TestTree]
test_runMergeGraphPure =
    [ testGroup
        "merge order"
        [ testCase "path"
        $   fst (runMergeGraphPure' (path [v "a", v "b", v "c"]))
        @?= [CheckoutRecord "c"]
        , testCase "diamond"
        $   fst
                (runMergeGraphPure'
                    (overlay (path [v "a", v "b", v "d"])
                             (path [v "a", v "c", v "d"])
                    )
                )
        @?= [CheckoutRecord "d"]
        , testCase "fork"
        $   fst
                (runMergeGraphPure'
                    (overlay (path [v "a", v "b"]) (path [v "a", v "c"]))
                )
        @?= [MergeRecord "b" "c" "b+c", CheckoutRecord "b+c"]
        , testCase "thee unrelated braches"
        $   fst (runMergeGraphPure' (vertices [v "a", v "b", v "c"]))
        @?= [ MergeRecord "a"   "b" "a+b"
            , MergeRecord "a+b" "c" "a+b+c"
            , CheckoutRecord "a+b+c"
            ]
        ]
    , testGroup
        "merge data"
        [ testCase "features"
        $   (fmap (Set.map yamlFeatureName . yamlFeatures) . snd)
                (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (Set.fromList ["a", "b"])
        , testCase "variables"
        $   (fmap yamlVariables . snd)
                (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (Map.fromList [("a", "a"), ("b", "b")])
        , testCase "conflicts for different vertices"
        $   (fmap yamlConflicts . snd)
                (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (Set.fromList ["a", "b"])
        , testCase "conflicts for a path"
        $ (fmap yamlConflicts . snd) (runMergeGraphPure' (path [v "a", v "b"]))
        @?= Just (Set.fromList ["b"])
        ]
    , testCase "cycle"
        $   runMergeGraphPure' (path [v "a", v "b", v "c", v "a"])
        @?= ([], Nothing)
    ]
    where runMergeGraphPure' graph = runMergeGraphPure graph (vertexSet graph)
