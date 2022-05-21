module Tenpureto.FeatureMergerSpec
    ( spec
    ) where

import           Algebra.Graph.ToGraph
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Test.Syd

import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.FeatureMerger
import           Tenpureto.Graph
import qualified Tenpureto.OrderedMap          as OrderedMap
import qualified Tenpureto.OrderedSet          as OrderedSet
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                )
import           Tenpureto.TemplateLoader.Internal
                                                ( FeatureStability(..)
                                                , TemplateYaml(..)
                                                , TemplateYamlFeature(..)
                                                )

spec :: Spec
spec = describe "runMergeGraphPure" $ do
    describe "merge order" $ do
        it "path" $ do
            fst (runMergeGraphPure' (path [v "a", v "b", v "c"]))
                `shouldBe` [CheckoutRecord "c"]
        it "diamond" $ do
            fst
                    (runMergeGraphPure'
                        (overlay (path [v "a", v "b", v "d"])
                                 (path [v "a", v "c", v "d"])
                        )
                    )
                `shouldBe` [CheckoutRecord "d"]
        it "fork" $ do
            fst
                    (runMergeGraphPure'
                        (overlay (path [v "a", v "b"]) (path [v "a", v "c"]))
                    )
                `shouldBe` [MergeRecord "b" "c" "b+c", CheckoutRecord "b+c"]
        it "thee unrelated braches" $ do
            fst (runMergeGraphPure' (vertices [v "a", v "b", v "c"]))
                `shouldBe` [ MergeRecord "a"   "b" "a+b"
                           , MergeRecord "a+b" "c" "a+b+c"
                           , CheckoutRecord "a+b+c"
                           ]
    describe "merged yaml data" $ do
        it "features" $ do
            (fmap (Set.map yamlFeatureName . yamlFeatures) . yamlResult)
                    (runMergeGraphPure' (vertices [v "a", v "b"]))
                `shouldBe` Just (Set.fromList ["a", "b"])
        it "variables" $ do
            (fmap yamlVariables . yamlResult)
                    (runMergeGraphPure' (vertices [v "a", v "b"]))
                `shouldBe` Just (OrderedMap.fromList [("a", "a"), ("b", "b")])
        it "conflicts for different vertices" $ do
            (fmap yamlConflicts . yamlResult)
                    (runMergeGraphPure' (vertices [v "a", v "b"]))
                `shouldBe` Just (Set.fromList ["a", "b"])
        it "conflicts for a path" $ do
            (fmap yamlConflicts . yamlResult)
                    (runMergeGraphPure' (path [v "a", v "b"]))
                `shouldBe` Just (Set.fromList ["b"])
    describe "merge heads" $ do
        it "for two different vertices" $ do
            headsResult (runMergeGraphPure' (vertices [v "a", v "b"]))
                `shouldBe` Just
                               (OrderedSet.fromList
                                   [Committish "a", Committish "b"]
                               )
        it "for a path" $ do
            headsResult (runMergeGraphPure' (path [v "a", v "b"]))
                `shouldBe` Just (OrderedSet.fromList [Committish "b"])
        it "cycle" $ do
            runMergeGraphPure' (path [v "a", v "b", v "c", v "a"])
                `shouldBe` ([], Nothing)
  where
    runMergeGraphPure' graph = runMergeGraphPure graph (vertexSet graph)
    yamlResult  = fmap fst . snd
    headsResult = fmap snd . snd

v :: Text -> TemplateBranchInformation
v c = TemplateBranchInformation
    { branchName   = c
    , branchCommit = Committish c
    , templateYaml = TemplateYaml
                         { yamlVariables = OrderedMap.singleton c c
                         , yamlExcludes  = Set.singleton c
                         , yamlConflicts = Set.singleton c
                         , yamlFeatures  = Set.singleton $ TemplateYamlFeature
                                               { yamlFeatureName        = c
                                               , yamlFeatureDescription = Nothing
                                               , yamlFeatureHidden      = False
                                               , yamlFeatureStability   = Stable
                                               }
                         }
    }
