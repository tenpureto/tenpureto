{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UI
    ( module UI
    , MonadConsole
    )
where

import           Data
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Bool
import           Data.Maybe
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Foldable
import           Data.Functor
import           Text.Printf
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Console
import           Path
import           Path.IO

import           Git                            ( Committish(..) )
import           Tenpureto.TemplateLoader       ( TemplateInformation(..)
                                                , TemplateBranchInformation(..)
                                                , isBaseBranch
                                                , isFeatureBranch
                                                , findTemplateBranch
                                                )

data UIException = UnattendedNotPossibleException | InvalidInputException | InterruptedInputException deriving (Exception)

instance Show UIException where
    show UnattendedNotPossibleException =
        "Running in an unattended mode, but some input required"
    show InvalidInputException     = "Invalid input"
    show InterruptedInputException = "Interrupted"

preSelectedBaseBranch
    :: TemplateInformation -> PreliminaryProjectConfiguration -> Maybe Text
preSelectedBaseBranch templateInformation providedConfiguration =
    let baseBranches = Set.fromList $ map branchName $ filter
            isBaseBranch
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
            (not . isBaseBranch)
            (branchesInformation templateInformation)
    in  fmap (Set.intersection featureBranches)
             (preSelectedBranches providedConfiguration)

unattendedTemplateConfiguration
    :: (MonadThrow m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
unattendedTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Just t, preTargetDirectory = Just td }
    = return FinalTemplateConfiguration { selectedTemplate = t
                                        , targetDirectory  = td
                                        }
unattendedTemplateConfiguration _ = throwM UnattendedNotPossibleException

unattendedUpdateConfiguration
    :: (MonadThrow m)
    => PreliminaryProjectConfiguration
    -> m FinalUpdateConfiguration
unattendedUpdateConfiguration PreliminaryProjectConfiguration { prePreviousTemplateCommit = Just c }
    = return FinalUpdateConfiguration { previousTemplateCommit = c }
unattendedUpdateConfiguration _ = throwM UnattendedNotPossibleException

unattendedProjectConfiguration
    :: (MonadThrow m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
unattendedProjectConfiguration templateInformation providedConfiguration =
    let bb = preSelectedBaseBranch templateInformation providedConfiguration
        fb =
                Set.toList
                    <$> preSelectedFeatureBranches templateInformation
                                                   providedConfiguration
        bbi = bb >>= findTemplateBranch templateInformation
        fbi = fb >>= traverse (findTemplateBranch templateInformation)
        bis = (:) <$> bbi <*> fbi
        v   = preVariableValues providedConfiguration
        cfg = FinalProjectConfiguration <$> bis <*> v
    in  maybe (throwM UnattendedNotPossibleException) return cfg

inputTemplate :: MonadConsole m => m Text
inputTemplate = ask "Template URL" Nothing

inputTarget :: (MonadIO m, MonadMask m, MonadConsole m) => m (Path Abs Dir)
inputTarget = T.unpack <$> ask "Target directory" Nothing >>= resolveTargetDir

inputPreviousCommit :: MonadConsole m => m Committish
inputPreviousCommit = Committish <$> ask "Previous template commit" Nothing

resolveTargetDir :: (MonadIO m, MonadCatch m) => FilePath -> m (Path Abs Dir)
resolveTargetDir path = catch
    (resolveDir' path)
    (\e -> let _ = (e :: PathException) in parseAbsDir path)

withDefaults :: Ord a => InsOrdHashMap a b -> Map a b -> InsOrdHashMap a b
withDefaults vars defaults =
    InsOrdHashMap.mapWithKey (\k v -> fromMaybe v (Map.lookup k defaults)) vars

inputTemplateConfiguration
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
inputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt, preTargetDirectory = mbtd }
    = FinalTemplateConfiguration
        <$> maybe inputTemplate return mbt
        <*> maybe inputTarget   return mbtd

inputUpdateConfiguration
    :: (MonadThrow m, MonadConsole m)
    => PreliminaryProjectConfiguration
    -> m FinalUpdateConfiguration
inputUpdateConfiguration PreliminaryProjectConfiguration { prePreviousTemplateCommit = mbc }
    = FinalUpdateConfiguration <$> maybe inputPreviousCommit return mbc

inputBranch
    :: (MonadIO m, MonadConsole m)
    => Doc AnsiStyle
    -> Doc AnsiStyle
    -> [TemplateBranchInformation]
    -> Set Text
    -> m (Maybe Text)
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

toggleBranch :: Text -> Set Text -> Set Text
toggleBranch branch selected = bool (Set.insert branch selected)
                                    (Set.delete branch selected)
                                    (Set.member branch selected)

inputBaseBranch
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => [TemplateBranchInformation]
    -> Maybe Text
    -> m Text
inputBaseBranch [single] _        = return (branchName single)
inputBaseBranch branches selected = do
    selection <- inputBranch "Base branches"
                             "Select a base branch"
                             branches
                             (maybe Set.empty Set.singleton selected)
    case selection of
        Just branch -> return branch
        Nothing     -> maybe (inputBaseBranch branches selected) return selected

inputFeatureBranches
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => [TemplateBranchInformation]
    -> Set Text
    -> m (Set Text)
inputFeatureBranches branches selected = do
    selection <- inputBranch "Feature branches"
                             "Add or remove a feature branch"
                             branches
                             selected
    case selection of
        Just branch ->
            inputFeatureBranches branches (toggleBranch branch selected)
        Nothing -> return selected

inputVariable :: (MonadIO m, MonadConsole m) => (Text, Text) -> m (Text, Text)
inputVariable (desc, name) = (desc, ) <$> ask (pretty desc) (Just name)

inputVariables
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => InsOrdHashMap Text Text
    -> m (Map Text Text)
inputVariables vars =
    Map.fromList <$> traverse inputVariable (InsOrdHashMap.toList vars)

inputProjectConfiguration
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
inputProjectConfiguration templateInformation providedConfiguration = do
    let bi       = branchesInformation templateInformation
        bases    = filter isBaseBranch bi
        features = filter isFeatureBranch bi
        child base branch = Set.member base (requiredBranches branch)
            && not (isBaseBranch branch)
    base <- inputBaseBranch
        bases
        (preSelectedBaseBranch templateInformation providedConfiguration)
    let featuresForBase = filter (child base) features
        preSelectedFeatures =
            preSelectedFeatureBranches templateInformation providedConfiguration
    branches <- if null featuresForBase
        then return Set.empty
        else inputFeatureBranches featuresForBase
                                  (fromMaybe Set.empty preSelectedFeatures)
    let allBranches = Set.insert base branches
        sbi         = filter (flip Set.member allBranches . branchName) bi
        sbvars      = mconcat (map branchVariables sbi)
        cvars = fromMaybe Map.empty (preVariableValues providedConfiguration)
        vars        = withDefaults sbvars cvars
    varVals <- inputVariables vars
    return FinalProjectConfiguration { projectBranches = sbi
                                     , variableValues  = varVals
                                     }

data ConflictResolutionStrategy = AlreadyResolved | MergeTool

inputResolutionStrategy
    :: MonadConsole m
    => Path Abs Dir
    -> [Path Rel File]
    -> m ConflictResolutionStrategy
inputResolutionStrategy repo conflicts = do
    sayLn "The following files have merge conflicts:"
    traverse_ (\c -> sayLn ("  " <> pretty c)) conflicts
    sayLn $ "Repository path: " <> pretty repo
    result <- confirm "Run \"git mergetool\"" (Just True)
    return $ bool AlreadyResolved MergeTool result

confirm :: MonadConsole m => Doc AnsiStyle -> Maybe Bool -> m Bool
confirm request def = askUntil (request <+> "(y/n)?")
                               (fmap defAns def)
                               mapAnswer
  where
    mapAnswer x = case x of
        "y" -> Right True
        "n" -> Right False
        _   -> Left "Please answer \"y\" or \"n\"."
    defAns True  = "y"
    defAns False = "n"
