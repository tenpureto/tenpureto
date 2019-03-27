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
createProject projectConfiguration unattended = do
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
              mergedTemplateYaml <- prepareTemplate repository templateInformation finalProjectConfiguration
              let dst = targetDirectory finalTemplateConfiguration
                  templaterSettings = buildTemplaterSettings mergedTemplateYaml finalProjectConfiguration in do
                createDir dst
                project <- initRepository dst
                files <- copy templaterSettings (repositoryPath repository) (repositoryPath project)
                addFiles project files
                commit project (commitCreateMessage finalTemplateConfiguration)

updateProject
    :: (MonadIO m, MonadMask m, MonadGit m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
updateProject projectConfiguration unattended = return ()

parseTemplateYaml :: ByteString -> Maybe TemplateYaml
parseTemplateYaml yaml =
    let templateYaml :: Either Y.ParseException TemplateYaml
        templateYaml = Y.decodeEither' yaml
    in  rightToMaybe templateYaml

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
