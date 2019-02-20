module Tenpureto
    ( module Tenpureto
    , withClonedRepository
    )
where

import           Data.List
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO.Temp
import           Data
import           Git
import           UI

data TemplateBranchConfiguration = TemplateBranchConfiguration
    { branchName :: String
    , requiredBranches :: [String]
    , branchVariables :: [String]
    }

makeFinalProjectConfiguration
    :: (MonadThrow m, MonadException m, MonadIO m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
makeFinalProjectConfiguration True  = unattendedConfiguration
makeFinalProjectConfiguration False = inputConfiguration

createProject
    :: ( MonadIO m
       , MonadMask m
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
    finalProjectConfiguration <- makeFinalProjectConfiguration
        unattended
        projectConfiguration
    withRepository (RepositoryUrl $ selectedTemplate finalProjectConfiguration)
        $ prepareTemplate finalProjectConfiguration

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
