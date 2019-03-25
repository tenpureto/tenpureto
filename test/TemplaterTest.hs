{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TemplaterTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Set                      as Set
import           Data.Bifunctor
import           Control.Monad.Log
import           Path

import           Templater

test_translateFile :: IO [TestTree]
test_translateFile = discardLogging $ do
    settings <- compileSettings TemplaterSettings
        { templaterVariables = [("bbb-ccc-ddd", "xxx-yyy")]
        , templaterExcludes  = Set.empty
        }
    let move = first show . translateFile settings in return
                [ testCase "should replace in file name"
                $   move [relfile|a-bbb-ccc-ddd-e.txt|]
                @?= Right [relfile|a-xxx-yyy-e.txt|]
                , testCase "should replace in path"
                $   move [relfile|a/bbb/ccc/ddd/e/f.txt|]
                @?= Right [relfile|a/xxx/yyy/e/f.txt|]
                ]
