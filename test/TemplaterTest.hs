{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TemplaterTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Set                      as Set
import           Data.Bifunctor
import           Control.Monad.Log
import           Path
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap

import           Templater

test_translateFile :: IO [TestTree]
test_translateFile = discardLogging $ do
    settings <- compileSettings TemplaterSettings
        { templaterFromVariables = InsOrdHashMap.singleton "A" "bbb-ccc-ddd"
        , templaterToVariables   = InsOrdHashMap.singleton "A" "xxx-yyy"
        , templaterExcludes      = Set.empty
        }
    let move = first show . translateFile settings
    return
        [ testCase "should replace in file name"
        $   move [relfile|a-bbb-ccc-ddd-e.txt|]
        @?= Right [relfile|a-xxx-yyy-e.txt|]
        , testCase "should replace in path"
        $   move [relfile|a/bbb/ccc/ddd/e/f.txt|]
        @?= Right [relfile|a/xxx/yyy/e/f.txt|]
        ]

test_excludes :: [TestTree]
test_excludes =
    let
        assertMatches a b = assertBool
            ("expected pattern: " ++ show a ++ " to match file: " ++ show b)
            (compileExcludes (Set.singleton a) b)
        assertNotMatches a b = assertBool
            ("expected pattern: " ++ show a ++ " to match file: " ++ show b)
            (not $ compileExcludes (Set.singleton a) b)
    in
        [ testGroup
            "wildcard"
            [ testCase "should match a file" $ assertMatches "*" [relfile|a|]
            , testCase "should match a file in a dir"
                $ assertMatches "*" [relfile|a/b|]
            ]
        , testGroup
            "double wildcard"
            [ testCase "should match zero dirs"
                $ assertMatches "/a/**/b" [relfile|a/b|]
            , testCase "should match one dir"
                $ assertMatches "/a/**/b" [relfile|a/c/b|]
            , testCase "should match two dirs"
                $ assertMatches "/a/**/b" [relfile|a/c/d/b|]
            ]
        , testGroup
            "root dir"
            [ testCase "should match a file in a dir"
                $ assertMatches "/a/" [relfile|a/b|]
            , testCase "should not match a file"
                $ assertNotMatches "/a/" [relfile|a|]
            ]
        , testGroup
            "root path"
            [ testCase "should match a file in a dir"
                $ assertMatches "/a" [relfile|a/b|]
            , testCase "should match a file" $ assertMatches "/a" [relfile|a|]
            ]
        , testGroup
            "dir"
            [ testCase "should match a dir" $ assertMatches "a/" [relfile|a/b|]
            , testCase "should not match a file"
                $ assertNotMatches "a/" [relfile|b/a|]
            ]
        , testGroup
            "path"
            [ testCase "should match a file in a dir"
                $ assertMatches "a" [relfile|b/a|]
            , testCase "should match a file" $ assertMatches "a" [relfile|a/b|]
            ]
        , testCase "should not match invalid patterns"
            $ assertNotMatches "[" [relfile|a|]
        ]
