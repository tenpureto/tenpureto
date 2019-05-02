{-# LANGUAGE OverloadedStrings #-}

module Tenpureto.MergeOptimizerTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.SmallCheck
import           Test.SmallCheck.Series

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set

import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                )
import           Tenpureto.MergeOptimizer

branch :: Text -> [Text] -> TemplateBranchInformation
branch name deps = TemplateBranchInformation
    { branchName       = name
    , isBaseBranch     = False
    , requiredBranches = Set.insert name $ Set.fromList deps
    , branchVariables  = mempty
    , templateYaml     = mempty
    }

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
