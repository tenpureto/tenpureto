module Tenpureto
    ( module Tenpureto
    , withClonedRepository
    )
where

import           Data.List
import           Data.Maybe
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO.Temp
import           Data
import           Git
import           UI

makeFinalTemplateConfiguration
    :: (MonadThrow m, MonadException m, MonadIO m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
makeFinalTemplateConfiguration True  = unattendedTemplateConfiguration
makeFinalTemplateConfiguration False = inputTemplateConfiguration

makeFinalProjectConfiguration
    :: (MonadThrow m, MonadException m, MonadIO m)
    => Bool
    -> TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
makeFinalProjectConfiguration True  = unattendedProjectConfiguration
makeFinalProjectConfiguration False = inputProjectConfiguration

createProject
    :: ( MonadIO m
       , MonadMask m
       , MonadException n
       , MonadIO n
       , MonadMask n
       , MonadException m
       , MonadGit n
       )
    => (RepositoryUrl -> n () -> m ())
    -> PreliminaryProjectConfiguration
    -> Bool
    -> m ()
createProject withRepository projectConfiguration unattended = do
    finalTemplateConfiguration <- makeFinalTemplateConfiguration
        unattended
        projectConfiguration
    withRepository (RepositoryUrl $ selectedTemplate finalTemplateConfiguration)
        $ do
              templateInformation       <- loadTemplateInformation
              finalProjectConfiguration <- makeFinalProjectConfiguration
                  unattended
                  templateInformation
                  projectConfiguration
                  Nothing
              prepareTemplate finalProjectConfiguration

updateProject
    :: (MonadIO m, MonadMask m, MonadIO n, MonadMask n, MonadGit n)
    => (RepositoryUrl -> n () -> m ())
    -> PreliminaryProjectConfiguration
    -> Bool
    -> m ()
updateProject withRepository projectConfiguration unattended = return ()

prepareTemplate
    :: (MonadIO m, MonadMask m, MonadGit m) => FinalProjectConfiguration -> m ()
prepareTemplate configuration = do
    branches <- listBranches
    liftIO $ putStrLn $ intercalate ", " branches

loadBranchConfiguration
    :: (MonadIO m, MonadGit m) => String -> m (Maybe TemplateBranchInformation)
loadBranchConfiguration branch = return $ Just TemplateBranchInformation
    { branchName       = branch
    , requiredBranches = []
    , branchVariables  = []
    }

validBranch :: String -> Bool
validBranch branch = True

loadTemplateInformation :: (MonadIO m, MonadGit m) => m TemplateInformation
loadTemplateInformation = do
    branches             <- listBranches
    branchConfigurations <- traverse loadBranchConfiguration
        $ filter validBranch branches
    return $ TemplateInformation
        { branchesInformation = catMaybes branchConfigurations
        }
