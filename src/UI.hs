{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Data
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Bool
import           Data.Tuple
import           Data.Maybe
import           Data.List
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Foldable
import           Data.Functor
import           Text.Printf
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           System.Console.Byline

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
unattendedTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Just t }
    = return FinalTemplateConfiguration { selectedTemplate = t }
unattendedTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Nothing }
    = throwM UnattendedNotPossibleException

unattendedProjectConfiguration
    :: (MonadThrow m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
unattendedProjectConfiguration _ providedConfiguration currentConfiguration =
    let b =
                preSelectedBranches providedConfiguration
                    `mplus` fmap selectedBranches currentConfiguration
        v =
                preVariableValues providedConfiguration
                    `mplus` fmap variableValues currentConfiguration
        cfg = FinalProjectConfiguration <$> b <*> v
    in  maybe (throwM UnattendedNotPossibleException) return cfg

runUI :: (MonadIO m, MonadMask m) => Byline m a -> m a
runUI a = runByline a >>= maybe (throwM InterruptedInputException) return

required :: (Monad m) => m (Maybe a) -> m a
required input = input >>= maybe (required input) return

inputTemplate :: (MonadIO m, MonadMask m) => m String
inputTemplate = runUI $ T.unpack <$> ask "Template URL: " Nothing

inputTemplateConfiguration
    :: (MonadIO m, MonadMask m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
inputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt }
    = FinalTemplateConfiguration <$> maybe inputTemplate return mbt

inputBranch
    :: (MonadIO m, MonadMask m)
    => [TemplateBranchInformation]
    -> Set String
    -> m (Maybe String)
inputBranch availableBranches selected =
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
        branchLineName = text . T.pack . branchName
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
        indexToBranch :: Text -> Maybe String
        indexToBranch index = branchByIndex index <&> branchName
    in
        runUI $ do
            sayLn "Template branches"
            traverse_ (sayLn . branchLine) indexedBranches
            askUntil "Add/remove branch: " Nothing (return . validateInput)
                <&> indexToBranch

toggleBranch :: String -> Set String -> Set String
toggleBranch branch selected = bool (Set.insert branch selected)
                                    (Set.delete branch selected)
                                    (Set.member branch selected)

inputBranches
    :: (MonadIO m, MonadMask m)
    => [TemplateBranchInformation]
    -> Set String
    -> m (Set String)
inputBranches branches selected = do
    selection <- inputBranch branches selected
    case selection of
        Just branch -> inputBranches branches (toggleBranch branch selected)
        Nothing     -> return selected

inputProjectConfiguration
    :: (MonadIO m, MonadMask m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
inputProjectConfiguration templateInformation providedConfiguration currentConfiguration
    = do
        branches <- inputBranches (branchesInformation templateInformation)
                                  Set.empty
        return FinalProjectConfiguration { selectedBranches = toList branches
                                         , variableValues   = []
                                         }
