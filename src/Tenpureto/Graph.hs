module Tenpureto.Graph
    ( Graph
    , overlay
    , compose
    , vertex
    , vertices
    , edge
    , edges
    , path
    , mapVertices
    , filterVertices
    , filterMapVertices
    , graphRoots
    , reachableExclusiveSubgraph
    )
where

import  Data.Set                      ( Set )
import qualified Data.Set                      as Set
import           Data.Tree                      ( rootLabel )
import qualified Algebra.Graph                 as G
import           Algebra.Graph.ToGraph          ( ToVertex
                                                , ToGraph
                                                , toGraph
                                                , dfsForest
                                                , reachable
                                                )

data Ord a => Graph a = NormalizedGraph   (G.Graph a)
                      | DenormalizedGraph (G.Graph a) (G.Graph a)
    deriving (Show)

instance Ord a => Eq (Graph a) where
    x == y = toGraph x == toGraph y

instance Ord a => ToGraph (Graph a) where
    type ToVertex (Graph a) = a
    toGraph (NormalizedGraph   a) = a
    toGraph (DenormalizedGraph _ a) = a

unGraph :: Ord a => Graph a -> G.Graph a
unGraph (NormalizedGraph   a) = a
unGraph (DenormalizedGraph a _) = a

denormalizedGraph :: Ord a => G.Graph a -> Graph a
denormalizedGraph a = DenormalizedGraph a (normalize a)

overlay :: Ord a => Graph a -> Graph a -> Graph a
overlay x y = denormalizedGraph $ G.overlay (unGraph x) (unGraph y)

compose :: Ord a => Graph a -> Graph a -> Graph a
compose x y = denormalizedGraph $ G.compose (unGraph x) (unGraph y)

vertex :: Ord a => a -> Graph a
vertex = denormalizedGraph . G.vertex

vertices :: Ord a => [a] -> Graph a
vertices = denormalizedGraph . G.vertices

edge :: Ord a => a -> a -> Graph a
edge x y = denormalizedGraph $ G.edge x y

edges :: Ord a => [(a, a)] -> Graph a
edges = denormalizedGraph . G.edges

path :: Ord a => [a] -> Graph a
path = denormalizedGraph . G.path

mapVertices :: (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
mapVertices f = denormalizedGraph . fmap f . unGraph

filterVertices :: Ord a => (a -> Bool) -> Graph a -> Graph a
filterVertices f = filterMapVertices (\x -> if f x then Just x else Nothing)

filterMapVertices :: (Ord a, Ord b) => (a -> Maybe b) -> Graph a -> Graph b
filterMapVertices f x =
    denormalizedGraph
        $ (=<<) (maybe G.empty G.vertex . snd)
        $ filterEmptyVertices
        $ fmap zipf (unGraph x)
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

graphRoots :: Ord a => Graph a -> [a]
graphRoots g =
    let potentialRoots = fmap rootLabel (dfsForest g)
        transposedGraph = G.transpose (toGraph g)
        isRoot x = [x] == reachable x transposedGraph
    in filter isRoot potentialRoots

reachableExclusiveSubgraph :: Ord a => Set a -> Graph a -> Graph a
reachableExclusiveSubgraph vs g =
    let g' = unGraph g
        rvs = Set.fromList $ concatMap (flip reachable g') vs
        rvsExclusive = Set.difference rvs vs
    in filterVertices (flip Set.member rvsExclusive) g

-- Internal

filterEdges :: Ord a => ((a, a) -> Bool) -> G.Graph a -> G.Graph a
filterEdges predicate x = G.overlay
    (G.vertices $ G.vertexList x)
    (G.edges $ filter predicate (G.edgeList x))

removeLoops :: Ord a => G.Graph a -> G.Graph a
removeLoops = filterEdges (uncurry (/=))

subtractEdges :: Ord a => G.Graph a -> G.Graph a -> G.Graph a
subtractEdges x y =
    let yEdges   = G.edgeSet y
        yhasEdge = flip Set.member yEdges
    in  filterEdges (not . yhasEdge) x

removeTransitiveEdges :: Ord a => G.Graph a -> G.Graph a
removeTransitiveEdges g = g `subtractEdges` (recCompose (g `G.compose` g))
  where
    recCompose x =
        let y = G.overlay x (x `G.compose` g)
        in  if y == x then y else recCompose y

normalize :: Ord a => G.Graph a -> G.Graph a
normalize = removeTransitiveEdges . removeLoops