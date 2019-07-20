{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tenpureto.FeatureMerger
    ( MergeRecord(..)
    , withMergeCache
    , runMergeGraphPure
    , runMergeGraph
    , listMergeCombinations
    )
where

import           Polysemy
import           Polysemy.Output
import           Polysemy.State

import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Functor
import           Control.Monad
import           Algebra.Graph.ToGraph

import           Tenpureto.Messages
import           Tenpureto.Graph
import           Tenpureto.TemplateLoader
import           Tenpureto.MergeOptimizer
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.Effects.Terminal
import           Tenpureto.FeatureMerger.Internal

data MergeRecord = MergeRecord Text Text Text
    deriving (Show, Eq)

mergeCommits
    :: Members '[Git, UI, Terminal] r
    => GitRepository
    -> (Committish, Text, Tree Text)
    -> (Committish, Text, Tree Text)
    -> MergedBranchDescriptor
    -> Sem r (Committish, Text, Tree Text)
mergeCommits repo (b1c, b1n, b1t) (b2c, b2n, b2t) d = do
    let mergedTree = Node (mergedBranchName d) [b2t, b1t]
    checkoutBranch repo (unCommittish b1c) Nothing
    mergeResult <- mergeBranch repo MergeAllowFastForward (unCommittish b2c)
    c           <- case mergeResult of
        MergeSuccessCommitted   -> getCurrentHead repo
        MergeSuccessUncommitted -> do
            updateTemplateYaml
            commit
                repo
                (commitMergeMessage b2n b1n <> "\n\n" <> showTree mergedTree)
        MergeConflicts mergeConflicts -> do
            updateTemplateYaml
            resolve d mergeConflicts
            commit
                repo
                (commitMergeMessage b2n b1n <> "\n\n" <> showTree mergedTree)

    return (c, mergedBranchName d, mergedTree)
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
    updateTemplateYaml = writeAddFile
        repo
        templateYamlFile
        (formatTemplateYaml (descriptorToTemplateYaml d))

withMergeCache :: Sem (State MergeCache ': r) a -> Sem r a
withMergeCache = fmap snd . runState mempty

runMergeGraph
    :: Members '[Git, UI, Terminal, State MergeCache] r
    => GitRepository
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> Sem r (Maybe TemplateYaml)
runMergeGraph repo graph branchInformation = do
    mbd <- mergeBranchesGraph branchData
                              mergeCommitsCached
                              graph
                              branchInformation
    forM_ mbd $ \d -> do
        writeAddFile repo templateYamlFile (formatTemplateYaml d)
        commit repo commitUpdateTemplateYaml
    return mbd
  where
    mergeCommitsCached a b d = do
        cache <- get
        case Map.lookup (a, b) cache of
            Just r  -> return r
            Nothing -> do
                r <- mergeCommits repo a b d
                modify (Map.insert (a, b) r)
                return r
    branchData bi = (branchCommit bi, branchName bi, Leaf (branchName bi))

runMergeGraphPure
    :: Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> ([MergeRecord], Maybe TemplateYaml)
runMergeGraphPure graph selectedBranches =
    run
        . runFoldMapOutput pure
        $ let logMerges b1 b2 d =
                  let mc = mergedBranchName d
                  in  output (MergeRecord b1 b2 mc) $> mc
          in  mergeBranchesGraph branchName logMerges graph selectedBranches

listMergeCombinations
    :: Graph TemplateBranchInformation -> [Set TemplateBranchInformation]
listMergeCombinations graph =
    let selectable branch =
                not (isHiddenBranch branch) && isFeatureBranch branch
        nodes        = filter selectable $ vertexList graph
        combinations = subsequences nodes
        addAncestors = filter selectable . graphAncestors graph
        noConflicts selected =
                let conflicts :: Set Text
                    conflicts =
                            (maybe mempty yamlConflicts . snd . runMergeGraphPure graph)
                                selected
                    selectedNames = Set.map branchName selected
                in  Set.null (Set.intersection conflicts selectedNames)
    in  filter noConflicts
            $   Set.toList
            .   Set.fromList
            $   fmap Set.fromList
            $   filter (not . null)
            $   addAncestors
            <$> combinations
