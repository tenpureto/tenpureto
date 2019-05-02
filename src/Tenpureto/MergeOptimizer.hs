module Tenpureto.MergeOptimizer where

import qualified Data.Set                      as Set
import           Data.Graph

import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                )

reorderBranches :: [TemplateBranchInformation] -> [TemplateBranchInformation]
reorderBranches branches =
    let edge bi = (bi, branchName bi, Set.toList (requiredBranches bi))
        (graph, nodeFromVertex, _) = graphFromEdges (fmap edge branches)
        topo                       = topSort graph
        fst3 (a, _, _) = a
    in  fmap (fst3 . nodeFromVertex) topo
