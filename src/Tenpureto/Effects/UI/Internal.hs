{-# LANGUAGE TupleSections #-}

module Tenpureto.Effects.UI.Internal where

import           Polysemy

import           Data.Bool
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Functor
import           Data.Foldable
import           Text.Printf
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

branchesByNames
    :: TemplateInformation -> Set Text -> [TemplateBranchInformation]
branchesByNames templateInformation branches = filter
    (flip Set.member branches . branchName)
    (branchesInformation templateInformation)

inputBranch
    :: Members '[Terminal, TerminalInput] r
    => Doc AnsiStyle
    -> Doc AnsiStyle
    -> [TemplateBranchInformation]
    -> Set Text
    -> Sem r (Maybe Text)
inputBranch title request availableBranches selected =
    let
        indexedBranches :: [(Int, TemplateBranchInformation)]
        indexedBranches = [1 ..] `zip` availableBranches
        branchLineIndex :: Int -> String
        branchLineIndex = printf "%2d) "
        branchLineSelected :: TemplateBranchInformation -> Doc AnsiStyle
        branchLineSelected branch =
            let isSelected = Set.member (branchName branch) selected
            in  bool "  " " *" isSelected
        branchLineName :: TemplateBranchInformation -> Doc AnsiStyle
        branchLineName = pretty . branchName
        branchLine :: (Int, TemplateBranchInformation) -> Doc AnsiStyle
        branchLine (index, branch) =
            annotate (color Green) (branchLineSelected branch)
                <> pretty (branchLineIndex index)
                <> annotate (color White) (branchLineName branch)
        branchByIndex :: Text -> Maybe TemplateBranchInformation
        branchByIndex index =
            find (\x -> T.unpack index == (show . fst) x) indexedBranches
                <&> snd
        validateInput :: Text -> Either (Doc AnsiStyle) Text
        validateInput input = if T.null input || isJust (branchByIndex input)
            then Right input
            else Left (pretty input <> " is not a valid branch number.")
        indexToBranch :: Text -> Maybe Text
        indexToBranch index = branchByIndex index <&> branchName
    in
        do
            sayLn title
            traverse_ (sayLn . branchLine) indexedBranches
            askUntil (request <> ":") Nothing validateInput <&> indexToBranch

inputBaseBranch
    :: Members '[Terminal, TerminalInput] r
    => [TemplateBranchInformation]
    -> Maybe Text
    -> Sem r Text
inputBaseBranch [single] _        = return (branchName single)
inputBaseBranch branches selected = do
    selection <- inputBranch "Base branches"
                             "Select a base branch"
                             branches
                             (maybe Set.empty Set.singleton selected)
    case selection of
        Just branch -> return branch
        Nothing     -> maybe (inputBaseBranch branches selected) return selected

toggleBranch :: Text -> Set Text -> Set Text
toggleBranch branch selected = bool (Set.insert branch selected)
                                    (Set.delete branch selected)
                                    (Set.member branch selected)

inputFeatureBranches
    :: Members '[Terminal, TerminalInput] r
    => [TemplateBranchInformation]
    -> Set Text
    -> Sem r (Set Text)
inputFeatureBranches branches selected = do
    selection <- inputBranch "Feature branches"
                             "Add or remove a feature branch"
                             branches
                             selected
    case selection of
        Just branch ->
            inputFeatureBranches branches (toggleBranch branch selected)
        Nothing -> return selected

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
