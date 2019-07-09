{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tenpureto.MergeOptimizerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map

import           Tenpureto.Graph
import           Tenpureto.MergeOptimizer
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                )
import           Tenpureto.TemplateLoader.Internal
                                                ( templateInformation
                                                , FeatureStability(..)
                                                , TemplateYamlFeature(..)
                                                )
import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.Effects.FeatureMerger
import           Tenpureto.TemplateTestHelper


test_reorderBranches :: [TestTree]
test_reorderBranches =
    [ testCase "keep single branch"
        $   reorderBranches [branch "a" []]
        @?= [branch "a" []]
    , testCase "put merge before bases"
        $   head
                (reorderBranches [branch "a" [], branch "b" [], branch "c" ["a", "b"]]
                )
        @?= branch "c" ["a", "b"]
    , testCase "handle cycles"
        $   length (reorderBranches [branch "a" ["b"], branch "b" ["a"]])
        @?= 2
    ]

test_includeMergeBranches :: [TestTree]
test_includeMergeBranches =
    [ testCase "should include merges of two base branches"
        $   (branchName <$> includeMergeBranches
                (templateInformation [a, b, c, z])
                [a, b, c]
            )
        @?= (branchName <$> [a, b, c, z])
    , testCase "should include merges of three base branches"
        $   (branchName <$> includeMergeBranches
                (templateInformation [a, b, c, q])
                [a, b, c]
            )
        @?= (branchName <$> [a, b, c, q])
    , testCase "should not include merges with additional data"
        $   (branchName <$> includeMergeBranches
                (templateInformation [a, b, c, y])
                [a, b, c]
            )
        @?= (branchName <$> [a, b, c])
    , testCase "should not include useless merges"
        $   (branchName <$> includeMergeBranches
                (templateInformation [a, b, c, x])
                [a, b, c]
            )
        @?= (branchName <$> [a, b, c])
    ]
  where
    a = baseBranch "a"
    b = baseBranch "b"
    c = baseBranch "c"
    z = mergeBranch "z" [a, b]
    y = childBranch "y" [a, b]
    x = renamedBranch "x" a
    q = mergeBranch "q" [a, b, c]

v :: Text -> MergedBranchInformation
v c = MergedBranchInformation
    { mergedBranchCommit     = Committish c
    , mergedBranchDescriptor =
        MergedBranchDescriptor
            { mergedVariables = Map.singleton c c
            , mergedExcludes  = Set.singleton c
            , mergedConflicts = Set.singleton c
            , mergedFeatures  = Set.singleton $ TemplateYamlFeature
                                    { yamlFeatureName        = c
                                    , yamlFeatureDescription = Nothing
                                    , yamlFeatureHidden      = False
                                    , yamlFeatureStability   = Stable
                                    }
            }
    }

test_mergeGraph :: [TestTree]
test_mergeGraph =
    [ testGroup
        "merge order"
        [ testCase "path"
        $   fst (runMergeGraph (path [v "a", v "b", v "c"]))
        @?= ["merge b a 1", "merge c 1 2"]
        , testCase "diamond"
        $   fst
                (runMergeGraph
                    (overlay (path [v "a", v "b", v "d"])
                             (path [v "a", v "c", v "d"])
                    )
                )
        @?= ["merge b a 1", "merge c a 2", "merge d 1 3", "merge 3 2 4"]
        ]
    , testGroup
        "merge data"
        [ testCase "features"
        $   ( fmap
                    ( Set.map yamlFeatureName
                    . mergedFeatures
                    . mergedBranchDescriptor
                    )
            . snd
            )
                (runMergeGraph (path [v "a", v "b"]))
        @?= Just (Set.fromList ["a", "b"])
        , testCase "variables"
        $   (fmap (mergedVariables . mergedBranchDescriptor) . snd)
                (runMergeGraph (path [v "a", v "b"]))
        @?= Just (Map.fromList [("a", "a"), ("b", "b")])
        , testCase "conflicts"
        $   (fmap (mergedConflicts . mergedBranchDescriptor) . snd)
                (runMergeGraph (path [v "a", v "b"]))
        @?= Just (Set.fromList ["b"])
        ]
    , testCase "cycle"
        $   runMergeGraph (path [v "a", v "b", v "c", v "a"])
        @?= ([], Nothing)
    ]

runMergeGraph
    :: Graph MergedBranchInformation -> ([Text], Maybe MergedBranchInformation)
runMergeGraph = runPure . mergeGraph mergeCommits

runPure :: Sem '[FeatureMerger] a -> ([Text], a)
runPure = run . runFeatureMergerPure

runFeatureMergerPure :: Sem (FeatureMerger ': r) a -> Sem r ([Text], a)
runFeatureMergerPure =
    runFoldMapOutput pure . runListInput @Int [1 ..] . reinterpret2 \case
        MergeCommits b1 b2 _ -> do
            mc <- (T.pack . show . fromJust) <$> input
            output
                $  "merge "
                <> unCommittish b1
                <> " "
                <> unCommittish b2
                <> " "
                <> mc
            return $ Committish mc
