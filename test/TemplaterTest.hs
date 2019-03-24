{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TemplaterTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Bifunctor
import           Path

import           Templater

test_translateFile :: [TestTree]
test_translateFile =
    let settings = compileSettings TemplaterSettings
            { templaterVariables = Map.singleton "bbb-ccc-ddd" "xxx-yyy"
            , templaterExcludes  = Set.empty
            }
        move = first show . translateFile settings
    in  [ testCase "should replace in file name"
            $   move [relfile|a-bbb-ccc-ddd-e.txt|]
            @?= Right [relfile|a-xxx-yyy-e.txt|]
        , testCase "should replace in path"
            $   move [relfile|a/bbb/ccc/ddd/e/f.txt|]
            @?= Right [relfile|a/xxx/yyy/e/f.txt|]
        ]
