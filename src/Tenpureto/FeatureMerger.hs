{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tenpureto.FeatureMerger
    ( MergeRecord(..)
    , runMergeGraphPure
    , runMergeGraph
    , listMergeCombinations
    )
where

import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import qualified Data.Text                     as T
import           Data.Maybe
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
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
    -> (Committish, Tree Text)
    -> (Committish, Tree Text)
    -> MergedBranchDescriptor
    -> Sem r (Committish, Tree Text)
mergeCommits repo (b1c, b1n) (b2c, b2n) d = do
    checkoutBranch repo (unCommittish b1c) Nothing
    mergeResult <- mergeBranch repo (unCommittish b2c)
    writeAddFile repo
                 templateYamlFile
                 (formatTemplateYaml (descriptorToTemplateYaml d))
    case mergeResult of
        MergeSuccess                  -> return ()
        MergeConflicts mergeConflicts -> resolve d mergeConflicts
    let mergedTree = Node "*" [b1n, b2n]
    maybeC <- commit repo ("Merge\n\n" <> showTree mergedTree)
    return (fromMaybe b1c maybeC, mergedTree)
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

runMergeGraph
    :: Members '[Git, UI, Terminal] r
    => GitRepository
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> Sem r (Maybe TemplateYaml)
runMergeGraph repo = mergeBranchesGraph branchData (mergeCommits repo)
    where branchData bi = (branchCommit bi, Leaf (branchName bi))

runMergeGraphPure
    :: Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> ([MergeRecord], Maybe TemplateYaml)
runMergeGraphPure graph selectedBranches =
    run
        . runFoldMapOutput pure
        . runListInput @Int [1 ..]
        $ let logMerges b1 b2 _ = do
                  mc <- ("merge-" <>) . T.pack . show . fromJust <$> input
                  output $ MergeRecord b1 b2 mc
                  return mc
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
