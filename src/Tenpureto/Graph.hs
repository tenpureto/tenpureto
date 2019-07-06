{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Tenpureto.Graph where

import qualified Data.Set                      as Set
import qualified Algebra.Graph                 as G
import           Algebra.Graph.ToGraph          ( ToGraph )

newtype Graph a = Graph { unGraph :: G.Graph a }
    deriving (Show, Eq, ToGraph)

simplify :: Ord a => Graph a -> Graph a
simplify = Graph . G.simplify . unGraph

overlay :: Graph a -> Graph a -> Graph a
overlay (Graph x) (Graph y) = Graph $ G.overlay x y

compose :: Ord a => Graph a -> Graph a -> Graph a
compose (Graph x) (Graph y) = Graph $ G.compose x y

vertices :: [a] -> Graph a
vertices = Graph . G.vertices

edge :: a -> a -> Graph a
edge x y = Graph $ G.edge x y

edges :: [(a, a)] -> Graph a
edges = Graph . G.edges

path :: [a] -> Graph a
path = Graph . G.path

maybeVertex :: Maybe a -> Graph a
maybeVertex = Graph . maybe G.empty G.vertex

filterEdges :: Ord a => ((a, a) -> Bool) -> Graph a -> Graph a
filterEdges predicate (Graph x) = overlay
    (vertices $ G.vertexList x)
    (edges $ filter predicate (G.edgeList x))

filterVertices :: Ord a => (a -> Bool) -> Graph a -> Graph a
filterVertices f = filterMapVertices (\x -> if f x then Just x else Nothing)

filterMapVertices :: (Ord a, Ord b) => (a -> Maybe b) -> Graph a -> Graph b
filterMapVertices f (Graph x) =
    Graph $ (=<<) (maybe G.empty G.vertex . snd) $ filterEmptyVertices $ fmap
        zipf
        x
  where
    zipf z = (z, f z)
    removeIfEmpty (  _, Just _ ) acc = acc
    removeIfEmpty v@(_, Nothing) acc = removeVertexConnecting v acc
    filterEmptyVertices
        :: (Ord a, Ord b) => G.Graph (a, Maybe b) -> G.Graph (a, Maybe b)
    filterEmptyVertices g = foldr removeIfEmpty g (G.vertexList g)
    removeVertexConnecting v g =
        let ctx = G.context ((==) v) g
            join c = G.connect (G.vertices (G.inputs c))
                               (G.vertices (G.outputs c))
            g' = G.removeVertex v g
        in  maybe g' (G.overlay g' . join) ctx

removeLoops :: Ord a => Graph a -> Graph a
removeLoops = filterEdges (uncurry (/=))

subtractEdges :: Ord a => Graph a -> Graph a -> Graph a
subtractEdges x (Graph y) =
    let yEdges   = G.edgeSet y
        yhasEdge = flip Set.member yEdges
    in  filterEdges (not . yhasEdge) x

removeTransitiveEdges :: Ord a => Graph a -> Graph a
removeTransitiveEdges g = g `subtractEdges` (recCompose (g `compose` g))
  where
    recCompose x =
        let y = overlay x (x `compose` g) in if y == x then y else recCompose y

instance Functor Graph where
    fmap f = Graph . fmap f . unGraph

instance Applicative Graph where
    pure = Graph . pure
    Graph x <*> Graph y = Graph (x <*> y)
