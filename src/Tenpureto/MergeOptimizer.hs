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
import           Tenpureto.Effects.Git          ( Committish )


data MergedBranchInformation = MergedBranchInformation
    { mergedBranchCommit :: Committish
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

mergedBranchInformationToTemplateYaml :: MergedBranchInformation -> TemplateYaml
mergedBranchInformationToTemplateYaml =
    descriptorToTemplateYaml . mergedBranchDescriptor

templateBranchInformationData
    :: TemplateBranchInformation -> MergedBranchInformation
templateBranchInformationData bi = MergedBranchInformation
    { mergedBranchCommit     = branchCommit bi
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
    :: Monad m
    => (Committish -> Committish -> MergedBranchDescriptor -> m Committish)
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> m (Maybe TemplateYaml)
mergeBranchesGraph mergeCommits graph selectedBranches =
    fmap (fmap mergedBranchInformationToTemplateYaml)
        $ mergeGraph mergeCommits
        $ mapVertices templateBranchInformationData
        $ graphSubset vertexDecision graph
  where
    vertexDecision v | v `Set.member` selectedBranches = MustKeep
                     | isHiddenBranch v                = PreferDrop
                     | isFeatureBranch v               = MustDrop
                     | otherwise                       = PreferKeep

mergeGraph
    :: Monad m
    => (Committish -> Committish -> MergedBranchDescriptor -> m Committish)
    -> Graph MergedBranchInformation
    -> m (Maybe MergedBranchInformation)
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
                c <- mergeCommits (mergedBranchCommit b1)
                                  (mergedBranchCommit b2)
                                  d
                return $ MergedBranchInformation { mergedBranchCommit     = c
                                                 , mergedBranchDescriptor = d
                                                 }
    vcombine = foldM (combine vcombineD)
    hcombine = combine hcombineD
