{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tenpureto.Effects.FeatureMergerTest where

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
import           Tenpureto.Effects.FeatureMerger
import           Tenpureto.TemplateLoader.Internal
                                                ( FeatureStability(..)
                                                , TemplateYamlFeature(..)
                                                )
import           Tenpureto.Effects.Git

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
runMergeGraph = runPure . mergeGraph

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
