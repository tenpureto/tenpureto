{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Tenpureto where

import           Data.List
import           Data.Maybe
import           Data.Either.Combinators
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString                     as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Yaml                     as Y
import           Data
import           Git
import           Console
import           UI
import           Logging
import           Templater
import           Path
import           Path.IO

templateYamlFile :: Path Rel File
templateYamlFile = [relfile|.template.yaml|]

makeFinalTemplateConfiguration
    :: (MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
makeFinalTemplateConfiguration True  = unattendedTemplateConfiguration
makeFinalTemplateConfiguration False = inputTemplateConfiguration

makeFinalProjectConfiguration
    :: (MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
makeFinalProjectConfiguration True  = unattendedProjectConfiguration
makeFinalProjectConfiguration False = inputProjectConfiguration

makeFinalUpdateConfiguration
    :: (MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalUpdateConfiguration
makeFinalUpdateConfiguration True  = unattendedUpdateConfiguration
makeFinalUpdateConfiguration False = inputUpdateConfiguration

buildTemplaterSettings :: TemplateYaml -> FinalProjectConfiguration -> TemplaterSettings
buildTemplaterSettings TemplateYaml { variables = templateValues }
                       FinalProjectConfiguration { variableValues = values } = TemplaterSettings
        { templaterFromVariables = templateValues
        , templaterToVariables = (InsOrdHashMap.fromList . Map.toList) values
        , templaterExcludes = Set.empty
        }

createProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
createProject projectConfiguration unattended =
    withPreparedTemplate projectConfiguration unattended $ \template finalTemplateConfiguration templaterSettings ->
        let dst = targetDirectory finalTemplateConfiguration in do
                createDir dst
                project <- initRepository dst
                files <- copy templaterSettings (repositoryPath template) (repositoryPath project)
                addFiles project files
                commit project (commitCreateMessage finalTemplateConfiguration)
                return ()

updateProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
updateProject projectConfiguration unattended = do
    finalUpdateConfiguration <- makeFinalUpdateConfiguration unattended projectConfiguration
    logDebug $ "Final update configuration" <> line <> (indent 4 . pretty) finalUpdateConfiguration
    withPreparedTemplate projectConfiguration unattended $ \template finalTemplateConfiguration templaterSettings ->
        withRepository (targetDirectory finalTemplateConfiguration) $ \project ->
            withNewWorktree project (previousTemplateCommit finalUpdateConfiguration) $ \staging -> do
                files <- copy templaterSettings (repositoryPath template) (repositoryPath staging)
                addFiles staging files
                result <- commit staging (commitUpdateMessage finalTemplateConfiguration)
                case result of
                    Just (Committish c) -> do
                        logInfo $ "Updated template commit:" <+> pretty c
                        mergeBranch project c (const (return ()))
                        commit project (commitUpdateMergeMessage finalTemplateConfiguration)
                        return ()
                    Nothing ->
                        outputNoUpdates

withPreparedTemplate
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> (GitRepository -> FinalTemplateConfiguration -> TemplaterSettings -> m())
    -> m ()
withPreparedTemplate projectConfiguration unattended block = do
    logDebug $ "Preliminary project configuration:" <> line <> (indent 4 . pretty) projectConfiguration
    finalTemplateConfiguration <- makeFinalTemplateConfiguration
        unattended
        projectConfiguration
    withClonedRepository
            (RepositoryUrl $ selectedTemplate finalTemplateConfiguration)
        $ \repository -> do
                templateInformation <- loadTemplateInformation repository
                logDebug $ "Template information" <> line <> (indent 4 . pretty) templateInformation
                finalProjectConfiguration <- makeFinalProjectConfiguration
                    unattended
                    templateInformation
                    projectConfiguration
                logDebug $ "Final project configuration" <> line <> (indent 4 . pretty) finalProjectConfiguration
                mergedTemplateYaml <- prepareTemplate repository templateInformation finalProjectConfiguration
                logDebug $ "Merged template YAML" <> line <> (indent 4 . pretty) mergedTemplateYaml
                let templaterSettings = buildTemplaterSettings mergedTemplateYaml finalProjectConfiguration in
                    block repository finalTemplateConfiguration templaterSettings

parseTemplateYaml :: ByteString -> Maybe TemplateYaml
parseTemplateYaml yaml =
    let info :: Either Y.ParseException TemplateYaml
        info = Y.decodeEither' yaml
    in  rightToMaybe info

loadExistingProjectConfiguration :: (MonadGit m, MonadLog m) => Path Abs Dir -> m PreliminaryProjectConfiguration
loadExistingProjectConfiguration projectPath = withRepository projectPath $ \project -> do
    logInfo $ "Loading template configuration from" <+> pretty templateYamlFile
    templateYamlContent <- getWorkingCopyFile project templateYamlFile
    previousTemplateCommit <- findCommit project commitMessagePattern
    let yaml = templateYamlContent >>= parseTemplateYaml in
        return PreliminaryProjectConfiguration
                { preSelectedTemplate = Nothing
                , preTargetDirectory  = Just projectPath
                , prePreviousTemplateCommit = previousTemplateCommit
                , preSelectedBranches = fmap features yaml
                , preVariableValues   = fmap (Map.fromList . InsOrdHashMap.toList . variables) yaml
                }

loadBranchConfiguration
    :: (MonadThrow m, MonadGit m)
    => GitRepository
    -> Text
    -> m (Maybe TemplateBranchInformation)
loadBranchConfiguration repo branch = runMaybeT $ do
    descriptor <- MaybeT $ getBranchFile
        repo
        (T.pack "remotes/origin/" <> branch)
        templateYamlFile
    info <- MaybeT $ return $ parseTemplateYaml descriptor
    let fb = features info in return $ TemplateBranchInformation
        { branchName       = branch
        , isBaseBranch     = Set.size fb <= 1
        , requiredBranches = fb
        , branchVariables  = variables info
        , templateYaml     = info
        }

loadTemplateInformation
    :: (MonadThrow m, MonadGit m) => GitRepository -> m TemplateInformation
loadTemplateInformation repository = do
    baseBranches         <- listBranches repository "base/"
    featureBranches      <- listBranches repository "feature/"
    branchConfigurations <- traverse (loadBranchConfiguration repository)
        $ sort (baseBranches ++ featureBranches)
    return $ TemplateInformation
        { branchesInformation = catMaybes branchConfigurations
        }

prepareTemplate
    :: (MonadIO m, MonadGit m, MonadConsole m)
    => GitRepository
    -> TemplateInformation
    -> FinalProjectConfiguration
    -> m TemplateYaml
prepareTemplate repository template configuration =
    let branch = "template"
        resolve descriptor conflicts =
            if templateYamlFile `elem` conflicts
                then writeAddFile repository templateYamlFile (Y.encode descriptor) >> resolve descriptor (delete templateYamlFile conflicts)
                else inputResolutionStrategy (repositoryPath repository) conflicts >>= \case
                    AlreadyResolved -> return ()
                    MergeTool -> runMergeTool repository >> sayLn "Successfully merged."
        mergeTemplateBranch a b = let
            bi = find ((==) b . branchName) (branchesInformation template)
            d = maybe a ((<>) a . templateYaml) bi in do
                mergeBranch repository ("origin/" <> b) (resolve d)
                commit repository ("Merge " <> b)
                return d
        in do
            checkoutBranch repository (baseBranch configuration) branch
            foldlM mergeTemplateBranch mempty (featureBranches configuration)

commitCreateMessage :: FinalTemplateConfiguration -> Text
commitCreateMessage cfg = "Create from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMessage :: FinalTemplateConfiguration -> Text
commitUpdateMessage cfg = "Update from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMergeMessage :: FinalTemplateConfiguration -> Text
commitUpdateMergeMessage cfg = "Merge " <> selectedTemplate cfg

commitMessagePattern :: Text
commitMessagePattern = "^Template: .*$"
