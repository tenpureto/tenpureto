{-# LANGUAGE TupleSections #-}

module Tenpureto.GraphSpec
    ( spec
    ) where

import           Algebra.Graph.ToGraph
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
import           Data.Ix
import           Data.List
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Syd
import           Test.Syd.Hedgehog

import           Tenpureto.Graph

spec :: Spec
spec = describe "Graph" $ do
    describe "filterVertices" $ do
        it "keeps edges" $ do
            s (filterVertices ("b" /=) (path ["a", "b", "c"]))
                `shouldBe` path ["a", "c"]
        it "keeps disconnected vertices" $ do
            s (filterVertices ("b" /=) (vertices ["a", "b", "c"]))
                `shouldBe` vertices ["a", "c"]
    it "does not have triangles" $ property $ do
        g <- forAll $ genIntGraph (Range.linear 0 10)
        let vs = vertexList g
        []
            === [ (a, b, c)
                | a <- vs
                , b <- vs
                , c <- vs
                , hasEdge a b g && hasEdge b c g && hasEdge a c g
                ]
    it "does not have loops" $ property $ do
        g <- forAll $ genIntGraph (Range.linear 0 10)
        let vs = vertexList g
        [] === [ a | a <- vs, hasEdge a a g ]
    it "is acyclic" $ property $ do
        g <- forAll $ genIntGraph (Range.linear 0 10)
        Hedgehog.assert $ isAcyclic $ toGraph g
    describe "graphRoots" $ do
        it "calculates roots for a simple graph" $ do
            graphRoots (s (path ["a", "b"])) `shouldBe` ["a"]
        it "graph roots do not have incoming edges" $ property $ do
            g <- forAll $ genIntGraph (Range.linear 0 10)
            let vs = vertexList g
            let rs = graphRoots g
            _ <- annotateShow rs
            [] === [ (a, b) | a <- vs, b <- rs, hasEdge a b g ]
    describe "foldTopologically" $ do
        it "uses last in the path" $ do
            foldLast (path ["a", "b", "c"]) `shouldBe` Just (Set.fromList ["c"])
        it "merges all vertices" $ do
            foldSet (overlay (path ["a", "b", "d"]) (path ["a", "c", "d"]))
                `shouldBe` Just (Set.fromList ["a", "b", "c", "d"])
        it "merges disconnected graphs" $ do
            foldLast (overlay (path ["a", "b"]) (path ["c", "d"]))
                `shouldBe` Just (Set.fromList ["b", "d"])
        it "processes every vertex once" $ property $ do
            graph <- forAll $ genIntGraph (Range.linear 0 100)
            let graphVertices = vertexList graph
            let record a _ = modify (a :) $> ()
            let ignore _ = return ()
            (_, foldedVertices) <- runStateT
                (foldTopologically record ignore graph)
                []
            sort foldedVertices === sort graphVertices
    describe "graphSubset" $ do
        it "keeps parents even if dropping is prefered" $ do
            graphSubset
                    (\case
                        "b" -> MustKeep
                        _   -> PreferDrop
                    )
                    (path ["a", "b"])
                `shouldBe` path @Text ["a", "b"]
        it "keeps children of kept vertices if keeping is prefered" $ do
            graphSubset
                    (\case
                        "a" -> MustKeep
                        _   -> PreferKeep
                    )
                    (path ["a", "b"])
                `shouldBe` path @Text ["a", "b"]
        it "drops children of kept vertices if dropping is prefered" $ do
            graphSubset
                    (\case
                        "a" -> MustKeep
                        _   -> PreferDrop
                    )
                    (path ["a", "b"])
                `shouldBe` path @Text ["a"]
        it "handles interleaving PreferKeep and PreferDrop" $ do
            graphSubset
                    (\case
                        "a" -> MustKeep
                        "b" -> MustDrop
                        "c" -> PreferKeep
                        "d" -> PreferKeep
                        "f" -> PreferKeep
                        _   -> PreferDrop
                    )
                    (path ["a", "b", "c", "d", "e", "f"])
                `shouldBe` path @Text ["a"]
        it
                "keeps children of kept vertices if grandchildren dropped when keeping is prefered"
            $ do
                  graphSubset
                          (\case
                              "a" -> MustKeep
                              "b" -> PreferKeep
                              "c" -> PreferDrop
                              "d" -> MustDrop
                              _   -> MustDrop
                          )
                          (path ["a", "b", "c", "d"])
                      `shouldBe` path @Text ["a", "b"]
        it "drops children of dropped vertices" $ do
            graphSubset
                    (\case
                        "a" -> MustDrop
                        _   -> PreferKeep
                    )
                    (path ["a", "b"])
                `shouldBe` path @Text []
        it "drops children of both dropped and kept vertices" $ do
            graphSubset
                    (\case
                        "a" -> MustDrop
                        "b" -> MustKeep
                        _   -> PreferKeep
                    )
                    (overlay (path ["a", "c"]) (path ["b", "c"]))
                `shouldBe` path @Text ["b"]
        it "respects vertex decisions" $ property $ do
            graph <- forAll $ genIntGraph (Range.linear 0 100)
            let graphVertices = vertexList graph
            decisions <- forAll
                $ traverse (\v -> (v, ) <$> Gen.enumBounded) graphVertices
            let ancestors = graphAncestors
                    graph
                    (fst <$> filter ((==) MustKeep . snd) decisions)
            let overrides      = Map.fromList $ zip ancestors (repeat MustKeep)
            let finalDecisions = Map.union overrides (Map.fromList decisions)
            let subgraph = graphSubset
                    (flip (Map.findWithDefault PreferKeep) finalDecisions)
                    graph
            annotateShow subgraph
            let subgraphVertices = Set.fromList $ vertexList subgraph
            for_ (Map.toList finalDecisions) $ \case
                (v, MustDrop) ->
                    Hedgehog.assert $ Set.notMember v subgraphVertices
                (v, MustKeep) ->
                    Hedgehog.assert $ Set.member v subgraphVertices
                _ -> return ()
  where
    foldSet :: Graph Text -> Maybe (Set Text)
    foldSet = runIdentity . foldTopologically setVCombine setHCombine
    setVCombine x ys = pure $ Set.insert x (mconcat ys)
    setHCombine = pure . fold
    foldLast :: Graph Text -> Maybe (Set Text)
    foldLast = runIdentity . foldTopologically lastVCombine setHCombine
    lastVCombine x _ = pure $ Set.singleton x

genIntGraph :: MonadGen m => Range Int -> m (Graph Int)
genIntGraph r = genGraph $ (\x -> range (0, x)) <$> Gen.int r

genGraph :: (Ord a, MonadGen m) => m [a] -> m (Graph a)
genGraph genVertices = do
    vs <- genVertices
    es <- Gen.subsequence [ (a, b) | a <- vs, b <- vs ]
    return $ overlay (vertices vs) (edges es)

s :: Graph Text -> Graph Text
s = id
