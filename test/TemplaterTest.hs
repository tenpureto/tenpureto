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
        , templaterToVariables = InsOrdHashMap.singleton "A" "xxx-yyy"
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
