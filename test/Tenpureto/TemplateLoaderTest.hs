{-# LANGUAGE OverloadedStrings #-}

module Tenpureto.TemplateLoaderTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.SmallCheck
import           Test.SmallCheck.Series

import qualified Data.Set                      as Set

import           Tenpureto.TemplateTestHelper
import           Tenpureto.TemplateLoader

test_getBranchParents :: [TestTree]
test_getBranchParents =
    [ testCase "include parents"
        $   getBranchParents (TemplateInformation [a, b]) b
        @?= Set.fromList ["a"]
    , testCase "not include grand parents"
        $   getBranchParents (TemplateInformation [a, b, c]) c
        @?= Set.fromList ["b"]
    ]
  where
    a = branch "a" []
    b = branch "b" ["a"]
    c = branch "c" ["a", "b"]

test_getBranchChildren :: [TestTree]
test_getBranchChildren =
    [ testCase "include children"
        $   getBranchChildren (TemplateInformation [a, b]) a
        @?= Set.fromList ["b"]
    , testCase "not include grand children"
        $   getBranchChildren (TemplateInformation [a, b, c]) a
        @?= Set.fromList ["b"]
    ]
  where
    a = branch "a" []
    b = branch "b" ["a"]
    c = branch "c" ["a", "b"]

test_getTemplateBranches :: [TestTree]
test_getTemplateBranches =
    [ testCase "list child branches"
        $   getTemplateBranches [BranchFilterChildOf "a"]
                                (TemplateInformation [a, b, c])
        @?= [b]
    , testCase "list parent branches"
        $   getTemplateBranches [BranchFilterParentOf "c"]
                                (TemplateInformation [a, b, c])
        @?= [b]
    , testCase "apply multiple filters"
        $   getTemplateBranches
                [BranchFilterChildOf "a", BranchFilterParentOf "c"]
                (TemplateInformation [a, b, c, d])
        @?= [b]
    ]
  where
    a = branch "a" []
    b = branch "b" ["a"]
    c = branch "c" ["a", "b", "e"]
    d = branch "d" ["a"]
    e = branch "e" []
