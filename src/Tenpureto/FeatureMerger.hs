{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tenpureto.FeatureMerger
    ( MergeRecord(..)
    , PropagatePushMode(..)
    , PropagateFailure(..)
    , withMergeCache
    , runMergeGraphPure
    , runMergeGraph
    , listMergeCombinations
    , runPropagateGraph
    )
where

import           Polysemy
import           Polysemy.Output
import           Polysemy.State

import           Data.Either
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
runMergeGraph repo graph branches = do
    mbd <- mergeBranchesGraph branchData mergeCommitsCached graph branches
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

data PropagatePushMode = PropagatePushMerged | PropagatePushSeparately
data PropagateData = PropagateData { propagateCurrentCommit :: Committish
                                   , propagateUpstreamCommit :: Committish
                                   , propagateBranchName :: Text
                                   }
                        deriving (Eq, Ord, Show)
data PropagateFailure = PropagateFailure Text Text
                            deriving (Eq, Ord, Show)

runPropagateGraph
    :: Members '[Git, Terminal, State MergeCache] r
    => GitRepository
    -> PropagatePushMode
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> Sem r [Either PropagateFailure PushSpec]
runPropagateGraph repo mode graph branches =
    Set.toList
        <$> propagateBranchesGraph branchData
                                   (propagateOne mode)
                                   (propagateMerge mode)
                                   graph
                                   branches
  where
    branchData bi = PropagateData { propagateCurrentCommit  = branchCommit bi
                                  , propagateUpstreamCommit = branchCommit bi
                                  , propagateBranchName     = branchName bi
                                  }
    mergeOne mid a = do
        needsMerge <- gitDiffHasCommits repo
                                        (propagateCurrentCommit a)
                                        (propagateUpstreamCommit mid)
        if not needsMerge
            then return (propagateCurrentCommit mid, mempty)
            else do
                checkoutBranch repo
                               (unCommittish $ propagateCurrentCommit mid)
                               Nothing
                preMergeResult <- mergeBranch
                    repo
                    MergeAllowFastForward
                    (unCommittish $ propagateCurrentCommit a)
                let title = pullRequestBranchIntoBranchTitle
                        (propagateBranchName a)
                        (propagateBranchName mid)
                let success c =
                        ( c
                        , Set.singleton $ Right $ UpdateBranch
                            { sourceCommit     = c
                            , sourceRef = BranchRef $ propagateBranchName a
                            , destinationRef   = BranchRef
                                                     $ propagateBranchName mid
                            , pullRequestRef   = BranchRef
                                                 $  propagateBranchName a
                                                 <> "/"
                                                 <> propagateBranchName mid
                            , pullRequestTitle = title
                            }
                        )
                    failure =
                        ( propagateCurrentCommit mid
                        , Set.singleton $ Left $ PropagateFailure
                            (propagateBranchName a)
                            (propagateBranchName mid)
                        )
                case preMergeResult of
                    MergeSuccessCommitted   -> success <$> getCurrentHead repo
                    MergeSuccessUncommitted -> success <$> commit repo title
                    MergeConflicts _        -> failure <$ mergeAbort repo
    propagateOne PropagatePushSeparately mi a =
        let mid = mergedBranchMeta mi
        in  do
                (_, actions) <- mergeOne mid a
                return
                    ( PropagateData
                        { propagateCurrentCommit  = propagateCurrentCommit mid
                        , propagateUpstreamCommit = propagateUpstreamCommit mid
                        , propagateBranchName     = propagateBranchName mid
                        }
                    , actions
                    )
    propagateOne PropagatePushMerged mi a =
        let mid = mergedBranchMeta mi
        in  do
                (c, actions) <- mergeOne mid a
                return
                    ( PropagateData
                        { propagateCurrentCommit  = c
                        , propagateUpstreamCommit = propagateUpstreamCommit mid
                        , propagateBranchName     = propagateBranchName mid
                        }
                    , Set.filter isLeft actions
                    )
    propagateMerge PropagatePushMerged b =
        let
            bd        = mergedBranchMeta b
            needsPush = propagateCurrentCommit bd /= propagateUpstreamCommit bd
        in
            return $ Set.fromList
                [ Right $ UpdateBranch
                      { sourceCommit     = propagateCurrentCommit bd
                      , sourceRef        = BranchRef $ propagateBranchName bd
                      , destinationRef   = BranchRef $ propagateBranchName bd
                      , pullRequestRef   = BranchRef
                                           $  propagateBranchName bd
                                           <> "/"
                                           <> propagateBranchName bd
                      , pullRequestTitle = pullRequestBranchUpdateTitle
                                               (propagateBranchName bd)
                      }
                | needsPush
                ]
    propagateMerge PropagatePushSeparately _ = return mempty
