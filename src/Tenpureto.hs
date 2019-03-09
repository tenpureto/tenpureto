module Tenpureto where

import           Data.List
import           Data.Maybe
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO.Temp
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

loadBranchConfiguration
    :: (MonadIO m, MonadGit m) => String -> m (Maybe TemplateBranchInformation)
loadBranchConfiguration branch = return $ Just TemplateBranchInformation
    { branchName       = branch
    , requiredBranches = []
    , branchVariables  = []
    }

loadTemplateInformation
    :: (MonadIO m, MonadGit m) => GitRepository -> m TemplateInformation
loadTemplateInformation repository = do
    baseBranches         <- listBranches repository "base/"
    featureBranches      <- listBranches repository "feature/"
    branchConfigurations <- traverse loadBranchConfiguration
        $ sort (baseBranches ++ featureBranches)
    return $ TemplateInformation
        { branchesInformation = catMaybes branchConfigurations
        }
