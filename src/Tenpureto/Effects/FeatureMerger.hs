{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.FeatureMerger where

import           Polysemy

import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.List
import           Data.Set                       ( Set )
import           Data.Map                       ( Map )
import           Control.Monad

import           Tenpureto.Graph
import           Tenpureto.Messages
import           Tenpureto.TemplateLoader
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.Effects.Terminal

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

data FeatureMerger m a where
    MergeCommits ::Committish -> Committish -> MergedBranchDescriptor -> FeatureMerger m Committish

makeSem ''FeatureMerger

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

runFeatureMergerGit
    :: Members '[Git, UI, Terminal] r
    => GitRepository
    -> Sem (FeatureMerger ': r) a
    -> Sem r a
runFeatureMergerGit repo = interpret $ \case
    MergeCommits b1 b2 d -> do
        checkoutBranch repo (unCommittish b1) Nothing
        mergeResult <- mergeBranch repo (unCommittish b2)
        _           <- case mergeResult of
            MergeSuccess                  -> return ()
            MergeConflicts mergeConflicts -> resolve d mergeConflicts
        maybeC <- commit repo "Merge"
        return $ fromMaybe b1 maybeC
      where
        resolve _ [] = return ()
        resolve descriptor mergeConflicts =
            if templateYamlFile `elem` mergeConflicts
                then
                    writeAddFile
                            repo
                            templateYamlFile
                            (formatTemplateYaml
                                (descriptorToTemplateYaml descriptor)
                            )
                        >> resolve
                               descriptor
                               (delete templateYamlFile mergeConflicts)
                else
                    inputResolutionStrategy (repositoryPath repo) mergeConflicts
                        >>= \case
                                AlreadyResolved -> return ()
                                MergeTool ->
                                    runMergeTool repo >> sayLn mergeSuccess

mergeBranchesGraph
    :: Member FeatureMerger r
    => Graph TemplateBranchInformation
    -> Sem r (Maybe TemplateYaml)
mergeBranchesGraph =
    fmap (fmap mergedBranchInformationToTemplateYaml)
        . mergeGraph
        . mapVertices templateBranchInformationData

mergeGraph
    :: Member FeatureMerger r
    => Graph MergedBranchInformation
    -> Sem r (Maybe MergedBranchInformation)
mergeGraph = foldTopologically vcombine hcombine
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
