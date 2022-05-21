module TenpuretoSpec
    ( spec
    ) where

import           Test.Syd

import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.Messages
import qualified Tenpureto.OrderedMap          as OrderedMap

import           Tenpureto

spec :: Spec
spec = do
    describe "extractTemplateName" $ do
        it "should extract from create message"
            $ extractTemplateName (commitCreateMessage "foo/bar" ["1", "2"])
            `shouldBe` Just "foo/bar"
        it "should extract from update message"
            $ extractTemplateName (commitUpdateMessage "foo/bar" ["1", "2"])
            `shouldBe` Just "foo/bar"
        it "should extract from create message with additional text"
            $          extractTemplateName
                           (commitCreateMessage "foo/bar" ["1", "2"] <> "Foo foo foo")
            `shouldBe` Just "foo/bar"
        it "should not extract from other messages"
            $          extractTemplateName "A\n\nTemplate foo/bar"
            `shouldBe` Nothing
    describe "extractTemplateCommits" $ do
        it "should extract from create message"
            $ extractTemplateCommits (commitCreateMessage "foo/bar" ["1", "2"])
            `shouldBe` [Committish "1", Committish "2"]
        it "should extract empty list from create message"
            $          extractTemplateCommits (commitCreateMessage "foo/bar" [])
            `shouldBe` []
        it "should extract from update message"
            $ extractTemplateCommits (commitUpdateMessage "foo/bar" ["1", "2"])
            `shouldBe` [Committish "1", Committish "2"]
        it "should extract from create message with additional text"
            $          extractTemplateCommits
                           (commitCreateMessage "foo/bar" ["1", "2"] <> "Foo foo foo")
            `shouldBe` [Committish "1", Committish "2"]
        it "should not extract from other messages"
            $          extractTemplateCommits "A\n\nTemplate foo/bar"
            `shouldBe` []
    describe "templateNameDefaultReplacement" $ do
        it "should map ssh git url repo name"
            $          templateNameDefaultReplacement "git@github.com:x/foo.git"
                                                      "/tmp/bar"
            `shouldBe` OrderedMap.singleton "foo" "bar"
        it "should map http git url repo name"
            $ templateNameDefaultReplacement "https://github.com/x/foo.git"
                                             "/tmp/bar"
            `shouldBe` OrderedMap.singleton "foo" "bar"
        it "should map local git url repo name"
            $          templateNameDefaultReplacement "/tmp/foo" "/tmp/bar"
            `shouldBe` OrderedMap.singleton "foo" "bar"
