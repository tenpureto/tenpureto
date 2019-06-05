{-# LANGUAGE OverloadedStrings #-}

module Tenpureto.TemplateLoaderTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.SmallCheck
import           Test.SmallCheck.Series

import qualified Data.Set                      as Set

import           Tenpureto.TemplateTestHelper
import           Tenpureto.TemplateLoader

test_managedBranches :: [TestTree]
test_managedBranches =
    [ testCase "include base branches"
        $   managedBranches (TemplateInformation [a])
        @?= [a]
    , testCase "include child branches"
        $   managedBranches (TemplateInformation [a, b])
        @?= [a, b]
    , testCase "not include renamed branches"
        $   managedBranches (TemplateInformation [a, b, c])
        @?= [a, b]
    , testCase "include merge branches"
        $   managedBranches (TemplateInformation [a, b, d, e])
        @?= [a, b, d, e]
    ]
  where
    a = baseBranch "a"
    b = childBranch "b" [a]
    c = renamedBranch "c" b
    d = childBranch "d" [a]
    e = mergeBranch "e" [b, d]

test_getBranchParents :: [TestTree]
test_getBranchParents =
    [ testCase "include parents"
        $   getBranchParents (TemplateInformation [a, b]) b
        @?= Set.fromList ["a"]
    , testCase "not include grand parents"
        $   getBranchParents (TemplateInformation [a, b, c]) c
        @?= Set.fromList ["b"]
    , testCase "not include renamed parents"
        $   getBranchParents (TemplateInformation [a, b, f, c]) c
        @?= Set.fromList ["b"]
    , testCase "not include anonymous parents"
        $   getBranchParents (TemplateInformation [g, a]) a
        @?= Set.fromList []
    , testCase "not include an original branch for a renamed one"
        $   getBranchParents (TemplateInformation [a, b, f]) f
        @?= Set.fromList ["a"]
    , testCase "not include a renamed branch for an original one"
        $   getBranchParents (TemplateInformation [a, b, f]) b
        @?= Set.fromList ["a"]
    ]
  where
    a = baseBranch "a"
    b = childBranch "b" [a]
    c = childBranch "c" [b]
    d = childBranch "d" [a]
    e = mergeBranch "e" [b, d]
    f = renamedBranch "f" b
    g = anonymousBranch "g" []

test_getBranchChildren :: [TestTree]
test_getBranchChildren =
    [ testCase "include children"
        $   getBranchChildren (TemplateInformation [a, b]) a
        @?= Set.fromList ["b"]
    , testCase "not include renamed children"
        $   getBranchChildren (TemplateInformation [a, b, g]) a
        @?= Set.fromList ["b"]
    , testCase "include merges"
        $   getBranchChildren (TemplateInformation [a, e, f]) a
        @?= Set.fromList ["f"]
    , testCase "not include grand children"
        $   getBranchChildren (TemplateInformation [a, b, c]) a
        @?= Set.fromList ["b"]
    , testCase "not include an original branch children for a renamed one"
        $   getBranchChildren (TemplateInformation [a, b, d]) d
        @?= Set.fromList []
    , testCase "not include a renamed one for an original one"
        $   getBranchChildren (TemplateInformation [a, b, d]) a
        @?= Set.fromList ["b"]
    ]
  where
    a = baseBranch "a"
    b = childBranch "b" [a]
    c = childBranch "c" [a, b]
    d = renamedBranch "d" a
    e = baseBranch "e"
    f = mergeBranch "f" [a, e]
    g = renamedBranch "g" b

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
    a = baseBranch "a"
    b = childBranch "b" [a]
    d = childBranch "d" [a]
    e = baseBranch "e"
    c = childBranch "c" [b, e]
