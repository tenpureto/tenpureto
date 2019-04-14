{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TenpuretoTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Path

import           Data
import           Tenpureto

test_extractTemplateName :: [TestTree]
test_extractTemplateName =
    let cfg = FinalTemplateConfiguration { selectedTemplate = "foo/bar"
                                         , targetDirectory  = [absdir|/tmp|]
                                         }
    in  [ testCase "should extract from create message"
            $   extractTemplateName (commitCreateMessage cfg)
            @?= Just (selectedTemplate cfg)
        , testCase "should extract from update message"
            $   extractTemplateName (commitUpdateMessage cfg)
            @?= Just (selectedTemplate cfg)
        , testCase "should not extract from other messages"
            $   extractTemplateName "A\n\nTemplate foo/bar"
            @?= Nothing
        ]
