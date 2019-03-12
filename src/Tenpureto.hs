{-# LANGUAGE OverloadedStrings #-}

module Tenpureto where

import           Data.List
import           Data.Maybe
import           Data.Either.Combinators
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           System.IO.Temp
import qualified Data.Yaml                     as Y
import           Data
import           Git
import           Console
import           UI

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

createProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m)
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
              prepareTemplate finalProjectConfiguration


updateProject
    :: (MonadIO m, MonadMask m, MonadGit m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
updateProject projectConfiguration unattended = return ()

prepareTemplate
    :: (MonadIO m, MonadMask m, MonadGit m) => FinalProjectConfiguration -> m ()
prepareTemplate configuration = liftIO $ print configuration

parseTemplateYaml :: ByteString -> Maybe TemplateYaml
parseTemplateYaml yaml =
    let templateYaml :: Either Y.ParseException TemplateYaml
        templateYaml = Y.decodeEither' yaml
    in  rightToMaybe templateYaml

loadBranchConfiguration
    :: (MonadIO m, MonadGit m)
    => GitRepository
    -> Text
    -> m (Maybe TemplateBranchInformation)
loadBranchConfiguration repo branch = runMaybeT $ do
    descriptor <- MaybeT $ getBranchFile
        repo
        (T.pack "remotes/origin/" <> branch)
        ".template.yaml"
    templateYaml <- MaybeT $ return $ parseTemplateYaml descriptor
    return $ TemplateBranchInformation
        { branchName       = branch
        , requiredBranches = features templateYaml
        , branchVariables  = variables templateYaml
        }

loadTemplateInformation
    :: (MonadIO m, MonadGit m) => GitRepository -> m TemplateInformation
loadTemplateInformation repository = do
    baseBranches         <- listBranches repository "base/"
    featureBranches      <- listBranches repository "feature/"
    branchConfigurations <- traverse (loadBranchConfiguration repository)
        $ sort (baseBranches ++ featureBranches)
    return $ TemplateInformation
        { branchesInformation = catMaybes branchConfigurations
        }
