{-# LANGUAGE QuasiQuotes #-}

module TenpuretoTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Path

import           Tenpureto.Data
import           Tenpureto.Messages
import qualified Tenpureto.OrderedMap          as OrderedMap

import           Tenpureto

test_extractTemplateName :: [TestTree]
test_extractTemplateName =
    let cfg = FinalTemplateConfiguration { selectedTemplate = "foo/bar"
                                         , targetDirectory  = [absdir|/tmp|]
                                         }
    in  [ testCase "should extract from create message"
            $   extractTemplateName (commitCreateMessage "foo/bar")
            @?= Just (selectedTemplate cfg)
        , testCase "should extract from update message"
            $   extractTemplateName (commitUpdateMessage "foo/bar")
            @?= Just (selectedTemplate cfg)
        , testCase "should extract from create message with additional text"
            $ extractTemplateName (commitCreateMessage "foo/bar" <> "\nFoo foo foo")
            @?= Just (selectedTemplate cfg)
        , testCase "should not extract from other messages"
            $   extractTemplateName "A\n\nTemplate foo/bar"
            @?= Nothing
        ]

test_templateNameDefaultReplacement :: [TestTree]
test_templateNameDefaultReplacement =
    [ testCase "should map ssh git url repo name"
        $   templateNameDefaultReplacement "git@github.com:x/foo.git" "/tmp/bar"
        @?= OrderedMap.singleton "foo" "bar"
    , testCase "should map http git url repo name"
        $ templateNameDefaultReplacement "https://github.com/x/foo.git" "/tmp/bar"
        @?= OrderedMap.singleton "foo" "bar"
    , testCase "should map local git url repo name"
        $   templateNameDefaultReplacement "/tmp/foo" "/tmp/bar"
        @?= OrderedMap.singleton "foo" "bar"
    ]
