{-# LANGUAGE OverloadedStrings #-}

module Tenpureto.MergeOptimizerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Tenpureto.TemplateTestHelper
import           Tenpureto.TemplateLoader       ( TemplateInformation(..)
                                                , TemplateBranchInformation(..)
                                                )
import           Tenpureto.MergeOptimizer

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
                (TemplateInformation [a, b, c, z])
                [a, b, c]
            )
        @?= (branchName <$> [a, b, c, z])
    , testCase "should not include merges with additional data"
        $   (branchName <$> includeMergeBranches
                (TemplateInformation [a, b, c, y])
                [a, b, c]
            )
        @?= (branchName <$> [a, b, c])
    , testCase "should not include useless merges"
        $   (branchName <$> includeMergeBranches
                (TemplateInformation [a, b, c, x])
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
