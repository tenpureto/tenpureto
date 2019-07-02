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
import           Control.Monad
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Tenpureto.Data
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

preSelectedBaseBranch
    :: TemplateInformation -> PreliminaryProjectConfiguration -> Maybe Text
preSelectedBaseBranch templateInformation providedConfiguration =
    let baseBranches = Set.fromList $ map branchName $ filter
            (isBaseBranch templateInformation)
            (branchesInformation templateInformation)
        selected =
                fromMaybe Set.empty (preSelectedBranches providedConfiguration)
        selectedBaseBranches  = Set.intersection baseBranches selected
        minSelectedBaseBranch = Set.lookupMin selectedBaseBranches
        maxSelectedBaseBranch = Set.lookupMax selectedBaseBranches
    in  if minSelectedBaseBranch == maxSelectedBaseBranch
            then minSelectedBaseBranch
            else Nothing

preSelectedFeatureBranches
    :: TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe (Set Text)
preSelectedFeatureBranches templateInformation providedConfiguration =
    let featureBranches = Set.fromList $ map branchName $ filter
            (not . isBaseBranch templateInformation)
            (branchesInformation templateInformation)
    in  fmap (Set.intersection featureBranches)
             (preSelectedBranches providedConfiguration)

templateBranchesByNames
    :: TemplateInformation -> Set Text -> [TemplateBranchInformation]
templateBranchesByNames templateInformation =
    filterBranchesByNames (branchesInformation templateInformation)

filterBranchesByNames
    :: [TemplateBranchInformation] -> Set Text -> [TemplateBranchInformation]
filterBranchesByNames availableBranches branches =
    filter (flip Set.member branches . branchName) availableBranches

type InputBranchState = (Set Text, Maybe Text)

branchSelection :: [TemplateBranchInformation] -> Set Text -> Set Text
branchSelection availableBranches selectedBranches =
    Set.unions
        $   requiredBranches
        <$> filterBranchesByNames availableBranches selectedBranches

inputBranchList :: [TemplateBranchInformation] -> Set Text -> Doc AnsiStyle
inputBranchList availableBranches selectedBranches = vsep $ zipWith6
    renderLine
    selectedPrefixes
    branchIndexes
    branchNames
    descriptions
    stabilities
    notes
  where
    renderLine
        :: Text
        -> Text
        -> Text
        -> Text
        -> FeatureStability
        -> Text
        -> Doc AnsiStyle
    renderLine selectedPrefix index branch description stability note =
        annotate (color Green) (pretty selectedPrefix)
            <+> pretty index
            <>  ")"
            <+> hang
                    4
                    (   annotate (color White) (pretty branch)
                    <>  softline
                    <>  softline
                    <>  annotate (color Green) (pretty description)
                    <+> stabilityNote stability
                    <>  annotate (color Black) (pretty note)
                    )
    stabilityNote Stable       = ""
    stabilityNote Experimental = annotate (color Yellow) ("(experimental)")
    stabilityNote Deprecated   = annotate (color Yellow) ("(deprecated)")
    equalize texts =
        let w = maximum (fmap T.length texts)
        in  fmap (T.justifyLeft w ' ') texts
    branchIndexes =
        equalize
            $     (T.pack . show . (fst @Int))
            <$>   [1 ..]
            `zip` availableBranches
    transitivelySelected = branchSelection availableBranches selectedBranches
    branchLineSelected branch =
        let name       = branchName branch
            selected   = name `Set.member` selectedBranches
            transitive = name `Set.member` transitivelySelected
        in  case (selected, transitive) of
                (True , _    ) -> (" *", "")
                (False, True ) -> (" +", "[transitive dependency]")
                (False, False) -> ("  ", "")
    (selectedPrefixes, notes) =
        unzip $ fmap branchLineSelected availableBranches
    branchNames  = equalize $ fmap branchName availableBranches
    descriptions = fmap
        (fromMaybe "" . (templateYamlFeature >=> featureDescription))
        availableBranches
    stabilities = fmap
        (maybe Stable featureStability . templateYamlFeature)
        availableBranches


branchByIndex
    :: [TemplateBranchInformation] -> Text -> Maybe TemplateBranchInformation
branchByIndex availableBranches index =
    let isIndex :: (Int, a) -> Bool
        isIndex = (==) index . T.pack . show . fst
    in  find isIndex ([1 ..] `zip` availableBranches) <&> snd

inputBaseBranch
    :: Members '[Terminal, TerminalInput] r
    => [TemplateBranchInformation]
    -> Maybe Text
    -> Sem r Text
inputBaseBranch availableBranches initialSelection = askUntil initial
                                                              request
                                                              process
  where
    initial :: Maybe Text
    initial = Nothing
    request :: Maybe Text -> (Doc AnsiStyle, Maybe Text)
    request badInput =
        let
            doc =
                "Base branches:\n"
                    <> inputBranchList
                           availableBranches
                           (Set.fromList (maybeToList initialSelection))
                    <> "\n"
                    <> maybe
                           ""
                           (\x ->
                               dquotes (pretty x)
                                   <+> "is not a valid branch index. "
                           )
                           badInput
                    <> "Select a base branch:"
        in  (doc, Nothing)
    process :: Maybe Text -> Text -> Either (Maybe Text) Text
    process _ input = case branchByIndex availableBranches input of
        Just bi -> Right $ branchName bi
        Nothing -> Left $ Just input

inputFeatureBranches
    :: Members '[Terminal, TerminalInput] r
    => [TemplateBranchInformation]
    -> Set Text
    -> Sem r (Set Text)
inputFeatureBranches availableBranches initialSelection = askUntil initial
                                                                   request
                                                                   process
  where
    initial :: InputBranchState
    initial = (initialSelection, Nothing)
    request :: InputBranchState -> (Doc AnsiStyle, Maybe Text)
    request (selected, badInput) =
        let
            doc =
                "Feature branches:\n"
                    <> inputBranchList availableBranches selected
                    <> "\n"
                    <> maybe
                           ""
                           (\x ->
                               dquotes (pretty x)
                                   <+> "is not a valid branch index. "
                           )
                           badInput
                    <> "Add or remove a feature branch:"
        in  (doc, Nothing)
    process :: InputBranchState -> Text -> Either InputBranchState (Set Text)
    process (selected, _) "" =
        Right $ branchSelection availableBranches selected
    process (selected, _) input =
        Left $ case branchByIndex availableBranches input of
            Just bi ->
                let branch      = branchName bi
                    newSelected = if branch `Set.member` selected
                        then branch `Set.delete` selected
                        else branch `Set.insert` selected
                in  (newSelected, Nothing)
            Nothing -> (selected, Just input)

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
