{-# LANGUAGE OverloadedStrings #-}

module Tenpureto.MergeOptimizerTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.SmallCheck
import           Test.SmallCheck.Series

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set

import           Tenpureto.TemplateLoader       ( TemplateInformation(..)
                                                , TemplateBranchInformation(..)
                                                )
import           Tenpureto.MergeOptimizer

branch :: Text -> [Text] -> TemplateBranchInformation
branch name deps = TemplateBranchInformation
    { branchName       = name
    , isBaseBranch     = False
    , isFeatureBranch  = False
    , requiredBranches = Set.insert name $ Set.fromList deps
    , branchVariables  = mempty
    , templateYaml     = mempty
    }

mergeBranch :: Text -> [Text] -> TemplateBranchInformation
mergeBranch name deps = TemplateBranchInformation
    { branchName       = name
    , isBaseBranch     = False
    , isFeatureBranch  = False
    , requiredBranches = Set.fromList deps
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

test_includeMergeBranches :: [TestTree]
test_includeMergeBranches =
    [ testCase "should include merges of two base branches"
        $   includeMergeBranches (TemplateInformation [a, b, c, z]) [a, b, c]
        @?= [a, b, c, z]
    , testCase "should not include merges with additional data"
        $   includeMergeBranches (TemplateInformation [a, b, c, y]) [a, b, c]
        @?= [a, b, c]
    , testCase "should not include useless merges"
        $   includeMergeBranches (TemplateInformation [a, b, c, x]) [a, b, c]
        @?= [a, b, c]
    ]
  where
    a = branch "a" []
    b = branch "b" []
    c = branch "c" []
    z = mergeBranch "z" ["a", "b"]
    y = branch "y" ["a", "b"]
    x = mergeBranch "x" ["a"]
