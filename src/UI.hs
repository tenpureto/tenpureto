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
import           Data.Tuple
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
import           Data.Monoid
import           Text.Printf
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Console
import           Path
import           Path.IO

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
        fb = preSelectedFeatureBranches templateInformation
                                        providedConfiguration
        v   = preVariableValues providedConfiguration
        cfg = FinalProjectConfiguration <$> bb <*> fb <*> v
    in  maybe (throwM UnattendedNotPossibleException) return cfg

required :: (Monad m) => m (Maybe a) -> m a
required input = input >>= maybe (required input) return

inputTemplate :: MonadConsole m => m Text
inputTemplate = ask "Template URL " Nothing

inputTarget :: (MonadIO m, MonadMask m, MonadConsole m) => m (Path Abs Dir)
inputTarget =
    T.unpack <$> ask "Target directory  " Nothing >>= resolveTargetDir

resolveTargetDir :: (MonadIO m, MonadCatch m) => FilePath -> m (Path Abs Dir)
resolveTargetDir path = catch
    (resolveDir' path)
    (\e -> let _ = (e :: PathException) in parseAbsDir path)

withDefaults :: Ord a => InsOrdHashMap a b -> Map a b -> InsOrdHashMap a b
withDefaults variables defaults = InsOrdHashMap.mapWithKey
    (\k v -> fromMaybe v (Map.lookup k defaults))
    variables

inputTemplateConfiguration
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
inputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt, preTargetDirectory = mbtd }
    = FinalTemplateConfiguration
        <$> maybe inputTemplate return mbt
        <*> maybe inputTarget   return mbtd

inputUpdateConfiguration
    :: (MonadThrow m)
    => PreliminaryProjectConfiguration
    -> m FinalUpdateConfiguration
inputUpdateConfiguration = unattendedUpdateConfiguration

inputBranch
    :: (MonadIO m, MonadConsole m)
    => Stylized
    -> Stylized
    -> [TemplateBranchInformation]
    -> Set Text
    -> m (Maybe Text)
inputBranch title request availableBranches selected =
    let
        indexedBranches :: [(Int, TemplateBranchInformation)]
        indexedBranches = [1 ..] `zip` availableBranches
        branchLineIndex :: Int -> Stylized
        branchLineIndex = text . T.pack . printf "%2d) "
        branchLineSelected :: TemplateBranchInformation -> Stylized
        branchLineSelected branch =
            let isSelected = Set.member (branchName branch) selected
            in  text $ T.pack $ bool "  " " *" isSelected
        branchLineName :: TemplateBranchInformation -> Stylized
        branchLineName = text . branchName
        branchLine :: (Int, TemplateBranchInformation) -> Stylized
        branchLine (index, branch) =
            (branchLineSelected branch <> fg green)
                <> branchLineIndex index
                <> (branchLineName branch <> fg white)
        branchByIndex :: Text -> Maybe TemplateBranchInformation
        branchByIndex index =
            find (\x -> T.unpack index == (show . fst) x) indexedBranches
                <&> snd
        validateInput :: Text -> Either Stylized Text
        validateInput input = if T.null input || isJust (branchByIndex input)
            then Right input
            else Left (text input <> " is not a valid branch number.")
        indexToBranch :: Text -> Maybe Text
        indexToBranch index = branchByIndex index <&> branchName
    in
        do
            sayLn title
            traverse_ (sayLn . branchLine) indexedBranches
            askUntil (request <> ": ") Nothing validateInput <&> indexToBranch

toggleBranch :: Text -> Set Text -> Set Text
toggleBranch branch selected = bool (Set.insert branch selected)
                                    (Set.delete branch selected)
                                    (Set.member branch selected)

inputBaseBranch
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => [TemplateBranchInformation]
    -> Maybe Text
    -> m Text
inputBaseBranch [single] selected = return (branchName single)
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
inputVariable (desc, name) = (desc, ) <$> ask (text $ desc <> " ") (Just name)

inputVariables
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => InsOrdHashMap Text Text
    -> m (Map Text Text)
inputVariables variables =
    Map.fromList <$> traverse inputVariable (InsOrdHashMap.toList variables)

inputProjectConfiguration
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
inputProjectConfiguration templateInformation providedConfiguration = do
    let bi    = branchesInformation templateInformation
        bases = filter isBaseBranch bi
        child base branch = Set.member base (requiredBranches branch)
            && not (isBaseBranch branch)
    base <- inputBaseBranch
        bases
        (preSelectedBaseBranch templateInformation providedConfiguration)
    let fbi = filter (child base) bi
        preSelectedFeatures =
            preSelectedFeatureBranches templateInformation providedConfiguration
    branches <- inputFeatureBranches
        fbi
        (fromMaybe Set.empty preSelectedFeatures)
    let sbi    = filter (flip Set.member branches . branchName) bi
        sbvars = mconcat (map branchVariables sbi)
        cvars  = fromMaybe Map.empty (preVariableValues providedConfiguration)
        vars   = withDefaults sbvars cvars
    varVals <- inputVariables vars
    return FinalProjectConfiguration { baseBranch      = base
                                     , featureBranches = branches
                                     , variableValues  = varVals
                                     }

data ConflictResolutionStrategy = AlreadyResolved | MergeTool

inputResolutionStrategy
    :: MonadConsole m
    => Path Abs Dir
    -> [Path Rel File]
    -> m ConflictResolutionStrategy
inputResolutionStrategy repo conflicts =
    let mapAnswer x = case x of
            "y" -> Right x
            "n" -> Right x
            _   -> Left "Please answer \"y\" or \"n\"."
    in
        do
            sayLn "The following files have merge conflicts:"
            traverse_ (\c -> sayLn ("  " <> (text . T.pack . toFilePath) c))
                      conflicts
            sayLn $ text $ "Repository path: " <> T.pack (toFilePath repo)
            result <- askUntil "Run \"git mergetool\" (y/n)? "
                               (Just "y")
                               mapAnswer
            return $ bool AlreadyResolved MergeTool (result == "y")
