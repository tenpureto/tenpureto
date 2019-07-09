{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.FeatureMerger where

import           Polysemy

import           Data.Maybe
import           Data.List

import           Tenpureto.Messages
import           Tenpureto.TemplateLoader
import           Tenpureto.MergeOptimizer
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.Effects.Terminal


data FeatureMerger m a where
    MergeCommits ::Committish -> Committish -> MergedBranchDescriptor -> FeatureMerger m Committish

makeSem ''FeatureMerger


runFeatureMergerGit
    :: Members '[Git, UI, Terminal] r
    => GitRepository
    -> Sem (FeatureMerger ': r) a
    -> Sem r a
runFeatureMergerGit repo = interpret $ \case
    MergeCommits b1 b2 d -> do
        checkoutBranch repo (unCommittish b1) Nothing
        mergeResult <- mergeBranch repo (unCommittish b2)
        writeAddFile repo
                     templateYamlFile
                     (formatTemplateYaml (descriptorToTemplateYaml d))
        case mergeResult of
            MergeSuccess                  -> return ()
            MergeConflicts mergeConflicts -> resolve d mergeConflicts
        maybeC <- commit repo "Merge"
        return $ fromMaybe b1 maybeC
      where
        resolve _ [] = return ()
        resolve descriptor mergeConflicts =
            if templateYamlFile `elem` mergeConflicts
                then resolve descriptor (delete templateYamlFile mergeConflicts)
                else
                    inputResolutionStrategy (repositoryPath repo) mergeConflicts
                        >>= \case
                                AlreadyResolved -> return ()
                                MergeTool ->
                                    runMergeTool repo >> sayLn mergeSuccess
