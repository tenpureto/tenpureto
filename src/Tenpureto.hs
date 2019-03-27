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
import qualified Data.Map                      as Map
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
    :: (MonadThrow m, MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
makeFinalTemplateConfiguration True  = unattendedTemplateConfiguration
makeFinalTemplateConfiguration False = inputTemplateConfiguration

makeFinalProjectConfiguration
    :: (MonadThrow m, MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
makeFinalProjectConfiguration True  = unattendedProjectConfiguration
makeFinalProjectConfiguration False = inputProjectConfiguration

buildTemplaterSettings :: TemplateYaml -> FinalProjectConfiguration -> TemplaterSettings
buildTemplaterSettings TemplateYaml {}
                       FinalProjectConfiguration { variableValues = values } = TemplaterSettings
        { templaterVariables = Map.toList values
        , templaterExcludes = Set.empty
        }

createProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
createProject projectConfiguration unattended =
    withPreparedTemplate projectConfiguration unattended $ \repository finalTemplateConfiguration templaterSettings ->
        let dst = targetDirectory finalTemplateConfiguration in do
                createDir dst
                project <- initRepository dst
                files <- copy templaterSettings (repositoryPath repository) (repositoryPath project)
                addFiles project files
                commit project (commitCreateMessage finalTemplateConfiguration)

updateProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
updateProject projectConfiguration unattended =
    withPreparedTemplate projectConfiguration unattended $ \repository finalTemplateConfiguration templaterSettings ->
        return ()

withPreparedTemplate :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
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
                templateInformation       <- loadTemplateInformation repository
                finalProjectConfiguration <- makeFinalProjectConfiguration
                    unattended
                    templateInformation
                    projectConfiguration
                    Nothing
                logDebug $ "Final project configuration" <> line <> (indent 4 . pretty) finalProjectConfiguration
                mergedTemplateYaml <- prepareTemplate repository templateInformation finalProjectConfiguration
                let templaterSettings = buildTemplaterSettings mergedTemplateYaml finalProjectConfiguration in
                    block repository finalTemplateConfiguration templaterSettings

parseTemplateYaml :: ByteString -> Maybe TemplateYaml
parseTemplateYaml yaml =
    let templateYaml :: Either Y.ParseException TemplateYaml
        templateYaml = Y.decodeEither' yaml
    in  rightToMaybe templateYaml

loadExistingProjectConfiguration :: (MonadIO m, MonadLog m) => Path Abs Dir -> m PreliminaryProjectConfiguration
loadExistingProjectConfiguration project = let yamlFile = project </> templateYamlFile in do
    logInfo $ "Loading template configuration from" <+> pretty yamlFile
    templateYamlResult <- liftIO $ try $ BS.readFile (toFilePath yamlFile)
    templateYamlContent <- case (templateYamlResult :: Either SomeException ByteString) of
        Right c -> return (Just c)
        Left e  -> do
            logInfo $ "Could not read" <+> pretty yamlFile <> ":" <+> (pretty . show) e
            return Nothing
    let yaml = templateYamlContent >>= parseTemplateYaml in
        return PreliminaryProjectConfiguration
                { preSelectedTemplate = Nothing
                , preTargetDirectory  = Just project
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
    templateYaml <- MaybeT $ return $ parseTemplateYaml descriptor
    let fb = features templateYaml in return $ TemplateBranchInformation
        { branchName       = branch
        , isBaseBranch     = Set.size fb <= 1
        , requiredBranches = fb
        , branchVariables  = variables templateYaml
        , templateYaml     = templateYaml
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
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m)
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
                return d
        in do
            checkoutBranch repository (baseBranch configuration) branch
            foldlM mergeTemplateBranch mempty (featureBranches configuration)

commitCreateMessage :: FinalTemplateConfiguration -> Text
commitCreateMessage cfg = "Create from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMessage :: FinalTemplateConfiguration -> Text
commitUpdateMessage cfg = "Update from a template\n\nTemplate: " <> selectedTemplate cfg
