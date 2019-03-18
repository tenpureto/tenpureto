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
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Text.Printf
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Console
import Path
import Path.IO

data UIException = UnattendedNotPossibleException | InvalidInputException | InterruptedInputException deriving (Exception)

instance Show UIException where
    show UnattendedNotPossibleException =
        "Running in an unattended mode, but some input required"
    show InvalidInputException     = "Invalid input"
    show InterruptedInputException = "Interrupted"

unattendedTemplateConfiguration
    :: (MonadThrow m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
unattendedTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Just t, preTargetDirectory = Just td }
    = return FinalTemplateConfiguration { selectedTemplate = t, targetDirectory = td }
unattendedTemplateConfiguration _
    = throwM UnattendedNotPossibleException

unattendedProjectConfiguration
    :: (MonadThrow m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
unattendedProjectConfiguration _ providedConfiguration currentConfiguration =
    let bb =
                preSelectedBaseBranch providedConfiguration
                    `mplus` fmap baseBranch currentConfiguration
        fb =
                preSelectedFeatureBranches providedConfiguration
                    `mplus` fmap featureBranches currentConfiguration
        v =
                preVariableValues providedConfiguration
                    `mplus` fmap variableValues currentConfiguration
        cfg = FinalProjectConfiguration <$> bb <*> fb <*> v
    in  maybe (throwM UnattendedNotPossibleException) return cfg

required :: (Monad m) => m (Maybe a) -> m a
required input = input >>= maybe (required input) return

inputTemplate :: (MonadIO m, MonadMask m, MonadConsole m) => m Text
inputTemplate = ask "Template URL " Nothing

inputTarget :: (MonadIO m, MonadMask m, MonadConsole m) => m (Path Abs Dir)
inputTarget = T.unpack <$> ask "Target directory  " Nothing >>= resolveTargetDir

resolveTargetDir :: (MonadIO m, MonadCatch m) => FilePath -> m (Path Abs Dir)
resolveTargetDir path = catch (resolveDir' path) (\e -> let _ = (e :: PathException) in parseAbsDir path)

inputTemplateConfiguration
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
inputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt, preTargetDirectory = mbtd }
    = FinalTemplateConfiguration <$> maybe inputTemplate return mbt <*> maybe inputTarget return mbtd

inputBranch
    :: (MonadIO m, MonadMask m, MonadConsole m)
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
            askUntil (request <> ": ") Nothing validateInput
                <&> indexToBranch

toggleBranch :: Text -> Set Text -> Set Text
toggleBranch branch selected = bool (Set.insert branch selected)
                                    (Set.delete branch selected)
                                    (Set.member branch selected)

inputBaseBranch :: (MonadIO m, MonadMask m, MonadConsole m) => [TemplateBranchInformation] -> Maybe Text -> m Text
inputBaseBranch branches selected = do
    selection <- inputBranch "Base branches" "Select a base branch" branches (maybe Set.empty Set.singleton selected)
    case selection of
        Just branch -> return branch
        Nothing     -> inputBaseBranch branches selected

inputFeatureBranches
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => [TemplateBranchInformation]
    -> Set Text
    -> m (Set Text)
inputFeatureBranches branches selected = do
    selection <- inputBranch "Feature branches" "Add or remove a feature branch" branches selected
    case selection of
        Just branch -> inputFeatureBranches branches (toggleBranch branch selected)
        Nothing     -> return selected

inputVariable
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => (Text, Text)
    -> m (Text, Text)
inputVariable (desc, name) = (name, ) <$> ask (text $ desc <> " ") (Just name)

inputVariables
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => Map Text Text
    -> m (Map Text Text)
inputVariables v = Map.fromList <$> traverse inputVariable (Map.assocs v)

inputProjectConfiguration
    :: (MonadIO m, MonadMask m, MonadConsole m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
inputProjectConfiguration templateInformation providedConfiguration currentConfiguration
    = let bi = branchesInformation templateInformation
          bases = filter isBaseBranch bi
          child base branch = Set.member base (requiredBranches branch) && not (isBaseBranch branch) in do
        base <- inputBaseBranch bases Nothing
        let fbi = filter (child base) bi in do
            branches <- inputFeatureBranches fbi Set.empty
            let sbi = filter (flip Set.member branches . branchName) bi
                vars = mconcat (map branchVariables sbi) in do
                varVals <- inputVariables vars
                return FinalProjectConfiguration
                    { baseBranch = base
                    , featureBranches = branches
                    , variableValues   = varVals
                    }

data ConflictResolutionStrategy = AlreadyResolved | MergeTool

inputResolutionStrategy :: MonadConsole m => Path Abs Dir -> [Path Rel File] -> m ConflictResolutionStrategy
inputResolutionStrategy repo conflicts = let
    mapAnswer x = case x of
        "y" -> Right x
        "n" -> Right x
        _ -> Left "Please answer \"y\" or \"n\"."
    in do
        sayLn "The following files have merge conflicts:"
        traverse_ (\c -> sayLn ("  " <> (text . T.pack . toFilePath) c)) conflicts
        sayLn $ text $ "Repository path: " <> T.pack (toFilePath repo)
        result <- askUntil "Run \"git mergetool\" (y/n)? " (Just "y") mapAnswer
        return $ bool AlreadyResolved MergeTool (result == "y")
