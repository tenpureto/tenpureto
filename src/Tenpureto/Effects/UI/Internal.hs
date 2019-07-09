{-# LANGUAGE TupleSections #-}

module Tenpureto.Effects.UI.Internal where

import           Polysemy

import           Data.Maybe
import           Data.List
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Functor
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Algebra.Graph.ToGraph

import           Tenpureto.Graph
import           Tenpureto.TemplateLoader
import           Tenpureto.Effects.Terminal
import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Git

inputTemplate :: Member TerminalInput r => Sem r Text
inputTemplate = ask "Template URL" Nothing

inputTarget :: Members '[TerminalInput, FileSystem] r => Sem r (Path Abs Dir)
inputTarget = ask "Target directory" Nothing <&> T.unpack >>= resolveDir

inputPreviousCommit :: Member TerminalInput r => Sem r Committish
inputPreviousCommit = ask "Previous template commit" Nothing <&> Committish

templateBranchesByNames
    :: TemplateInformation -> Set Text -> [TemplateBranchInformation]
templateBranchesByNames templateInformation =
    filterBranchesByNames (branchesInformation templateInformation)

filterBranchesByNames
    :: [TemplateBranchInformation] -> Set Text -> [TemplateBranchInformation]
filterBranchesByNames availableBranches branches =
    filter (flip Set.member branches . branchName) availableBranches

findBranchByName
    :: [TemplateBranchInformation] -> Text -> Maybe TemplateBranchInformation
findBranchByName availableBranches branch =
    find ((==) branch . branchName) availableBranches

branchSelection :: [TemplateBranchInformation] -> Set Text -> Set Text
branchSelection availableBranches selectedBranches =
    Set.unions
        $   requiredBranches
        <$> filterBranchesByNames availableBranches selectedBranches

inputBranchList
    :: Graph TemplateBranchInformation
    -> [TemplateBranchInformation]
    -> Set Text
    -> Doc AnsiStyle
inputBranchList _ availableBranches selectedBranches = vsep $ zipWith6
    renderLine
    selectedPrefixes
    branchIndexes
    branchNames
    descriptions
    stabilities
    notes
  where
    green  = annotate (color Green)
    red    = annotate (color Red)
    yellow = annotate (color Yellow)
    renderLine
        :: Doc AnsiStyle
        -> Text
        -> Text
        -> Text
        -> FeatureStability
        -> Text
        -> Doc AnsiStyle
    renderLine selectedPrefix index branch description stability note =
        " " <> selectedPrefix <+> pretty index <> ")" <+> hang
            4
            (   annotate (color White) (pretty branch)
            <>  softline
            <>  softline
            <>  annotate (color Green) (pretty description)
            <+> stabilityNote stability
            <>  annotate (color Black) (pretty note)
            )
    stabilityNote Stable       = ""
    stabilityNote Experimental = yellow "(experimental)"
    stabilityNote Deprecated   = yellow "(deprecated)"
    equalize texts =
        let w = maximum (fmap T.length texts)
        in  fmap (T.justifyLeft w ' ') texts
    branchIndexes =
        equalize $ T.pack . show . fst @Int <$> [1 ..] `zip` availableBranches
    transitivelySelected = branchSelection availableBranches selectedBranches
    conflictsWithSelected branch = any (branchesConflict branch) $ filter
        (flip Set.member selectedBranches . branchName)
        availableBranches
    branchLineSelected branch =
        let name        = branchName branch
            selected    = name `Set.member` selectedBranches
            transitive  = name `Set.member` transitivelySelected
            conflicting = conflictsWithSelected branch
        in  case (selected, transitive, conflicting) of
                (True , _    , _   ) -> (green "✓", "")
                (False, True , _   ) -> (green "·", "[transitive dependency]")
                (_    , _    , True) -> (red "𐄂", "[conflict]")
                (False, False, _   ) -> (" ", "")
    (selectedPrefixes, notes) =
        unzip $ fmap branchLineSelected availableBranches
    branchNames  = equalize $ fmap branchName availableBranches
    descriptions = fmap (fromMaybe "" . featureDescription) availableBranches
    stabilities  = fmap featureStability availableBranches

branchByIndex
    :: [TemplateBranchInformation] -> Text -> Maybe TemplateBranchInformation
branchByIndex availableBranches index =
    let isIndex :: (Int, a) -> Bool
        isIndex = (==) index . T.pack . show . fst
    in  find isIndex ([1 ..] `zip` availableBranches) <&> snd

type InputBranchState
    = ([TemplateBranchInformation], Set TemplateBranchInformation, Maybe Text)

inputBranches
    :: Members '[Terminal, TerminalInput] r
    => Graph TemplateBranchInformation
    -> Set Text
    -> Sem r (Set TemplateBranchInformation)
inputBranches graph initialNameSelection = askUntil initial request process
  where
    initialSelection :: Set TemplateBranchInformation
    initialSelection = Set.fromList
        $ filterBranchesByNames (vertexList graph) initialNameSelection
    roots :: Set TemplateBranchInformation
    roots = Set.fromList (graphRoots graph)
    available :: Set TemplateBranchInformation -> [TemplateBranchInformation]
    available selected =
        let fullSelection = filterBranchesByNames (vertexList graph)
                $ Set.unions (Set.map requiredBranches selected)
            reachableFromSelection = Set.unions
                $ fmap (Set.fromList . flip reachable graph) fullSelection
        in  Set.toList $ reachableFromSelection `Set.union` roots
    initial :: InputBranchState
    initial = (available initialSelection, initialSelection, Nothing)
    request :: InputBranchState -> (Doc AnsiStyle, Maybe Text)
    request (availableBranches, selected, badInput) =
        let
            doc =
                inputBranchList graph
                                availableBranches
                                (Set.map branchName selected)
                    <> "\n"
                    <> maybe
                           ""
                           (\x ->
                               dquotes (pretty x)
                                   <+> "is not a valid feature index. "
                           )
                           badInput
                    <> "Add or remove a feature:"
        in  (doc, Nothing)
    process
        :: InputBranchState
        -> Text
        -> Either InputBranchState (Set TemplateBranchInformation)
    process (_, selected, _) "" = Right selected
    process (availableBranches, selected, _) input =
        Left $ case branchByIndex availableBranches input of
            Just bi ->
                let newSelected = if bi `Set.member` selected
                        then bi `Set.delete` selected
                        else bi `Set.insert` selected
                in  (available newSelected, newSelected, Nothing)
            Nothing -> (availableBranches, selected, Just input)

withDefaults :: Ord a => InsOrdHashMap a b -> Map a b -> InsOrdHashMap a b
withDefaults vars defaults =
    InsOrdHashMap.mapWithKey (\k v -> fromMaybe v (Map.lookup k defaults)) vars

inputVariable :: Member TerminalInput r => (Text, Text) -> Sem r (Text, Text)
inputVariable (desc, name) = (desc, ) <$> ask (pretty desc) (Just name)

inputVariables
    :: Member TerminalInput r
    => InsOrdHashMap Text Text
    -> Sem r (Map Text Text)
inputVariables vars =
    Map.fromList <$> traverse inputVariable (InsOrdHashMap.toList vars)
