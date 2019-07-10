module Tenpureto.MergeOptimizer where

import           Data.Text                      ( Text )
import           Data.List
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Map                       ( Map )
import           Data.Graph                     ( graphFromEdges
                                                , topSort
                                                )
import           Control.Monad

import           Tenpureto.Graph
import           Tenpureto.TemplateLoader       ( TemplateInformation
                                                , TemplateBranchInformation(..)
                                                , TemplateYaml(..)
                                                , TemplateYamlFeature
                                                , managedBranches
                                                , isMergeOf
                                                , isFeatureBranch
                                                , isHiddenBranch
                                                , requiredBranches
                                                , branchVariables
                                                )

data MergedBranchInformation a = MergedBranchInformation
    { mergedBranchMeta :: a
    , mergedBranchDescriptor :: MergedBranchDescriptor
    }
    deriving (Show, Eq, Ord)

data MergedBranchDescriptor = MergedBranchDescriptor
    { mergedVariables :: Map Text Text
    , mergedExcludes :: Set Text
    , mergedConflicts :: Set Text
    , mergedFeatures :: Set TemplateYamlFeature
    }
    deriving (Show, Eq, Ord)

descriptorToTemplateYaml :: MergedBranchDescriptor -> TemplateYaml
descriptorToTemplateYaml d = TemplateYaml { yamlVariables = mergedVariables d
                                          , yamlFeatures  = mergedFeatures d
                                          , yamlExcludes  = mergedExcludes d
                                          , yamlConflicts = mergedConflicts d
                                          }

mergedBranchInformationToTemplateYaml
    :: MergedBranchInformation a -> TemplateYaml
mergedBranchInformationToTemplateYaml =
    descriptorToTemplateYaml . mergedBranchDescriptor

templateBranchInformationData
    :: (TemplateBranchInformation -> a)
    -> TemplateBranchInformation
    -> MergedBranchInformation a
templateBranchInformationData extract bi = MergedBranchInformation
    { mergedBranchMeta       = extract bi
    , mergedBranchDescriptor =
        MergedBranchDescriptor
            { mergedVariables = branchVariables bi
            , mergedExcludes  = (yamlExcludes . templateYaml) bi
            , mergedConflicts = (yamlConflicts . templateYaml) bi
            , mergedFeatures  = (yamlFeatures . templateYaml) bi
            }
    }

reorderBranches :: [TemplateBranchInformation] -> [TemplateBranchInformation]
reorderBranches branches =
    let mkEdge bi = (bi, branchName bi, Set.toList (requiredBranches bi))
        (graph, nodeFromVertex, _) = graphFromEdges (fmap mkEdge branches)
        topo                       = topSort graph
        fst3 (a, _, _) = a
    in  fmap (fst3 . nodeFromVertex) topo

includeMergeBranches
    :: TemplateInformation
    -> [TemplateBranchInformation]
    -> [TemplateBranchInformation]
includeMergeBranches template branches =
    let allBranches  = managedBranches template
        mergeOptions = filter ((<) 1 . length) (subsequences branches)
        isMerge bi = any (isMergeOf bi) mergeOptions
    in  nub $ branches ++ filter isMerge allBranches

mergeBranchesGraph
    :: (Ord a, Monad m)
    => (TemplateBranchInformation -> a)
    -> (a -> a -> MergedBranchDescriptor -> m a)
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> m (Maybe TemplateYaml)
mergeBranchesGraph extract mergeCommits graph selectedBranches =
    fmap (fmap mergedBranchInformationToTemplateYaml)
        $ mergeGraph mergeCommits
        $ mapVertices (templateBranchInformationData extract)
        $ graphSubset (vertexDecision selectedBranches) graph

vertexDecision
    :: Set TemplateBranchInformation
    -> TemplateBranchInformation
    -> GraphSubsetDecision
vertexDecision selectedBranches v | v `Set.member` selectedBranches = MustKeep
                                  | isHiddenBranch v                = PreferDrop
                                  | isFeatureBranch v               = MustDrop
                                  | otherwise                       = PreferKeep

mergeGraph
    :: (Ord a, Monad m)
    => (a -> a -> MergedBranchDescriptor -> m a)
    -> Graph (MergedBranchInformation a)
    -> m (Maybe (MergedBranchInformation a))
mergeGraph mergeCommits = foldTopologically vcombine hcombine
  where
    vcombineD d1 d2 = MergedBranchDescriptor
        { mergedVariables = mergedVariables d1 <> mergedVariables d2
        , mergedExcludes  = mergedExcludes d1 <> mergedExcludes d2
        , mergedConflicts = mergedConflicts d1
        , mergedFeatures  = mergedFeatures d1 <> mergedFeatures d2
        }
    hcombineD d1 d2 = MergedBranchDescriptor
        { mergedVariables = mergedVariables d1 <> mergedVariables d2
        , mergedExcludes  = mergedExcludes d1 <> mergedExcludes d2
        , mergedConflicts = mergedConflicts d1 <> mergedConflicts d2
        , mergedFeatures  = mergedFeatures d1 <> mergedFeatures d2
        }
    combine combineD b1 b2 =
        let d = combineD (mergedBranchDescriptor b1)
                         (mergedBranchDescriptor b2)
        in  do
                c <- mergeCommits (mergedBranchMeta b1) (mergedBranchMeta b2) d
                return $ MergedBranchInformation { mergedBranchMeta       = c
                                                 , mergedBranchDescriptor = d
                                                 }
    vcombine = foldM (combine vcombineD)
    hcombine = combine hcombineD
