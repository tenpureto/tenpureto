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

data TemplateBranchConfiguration = TemplateBranchConfiguration
    { branchName :: String
    , requiredBranches :: [String]
    , branchVariables :: [String]
    }

makeFinalProjectConfiguration
    :: PreliminaryProjectConfiguration -> FinalProjectConfiguration
makeFinalProjectConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Just t, preSelectedBranches = b, preVariableValues = v }
    = FinalProjectConfiguration { selectedTemplate = t
                                , selectedBranches = b
                                , variableValues   = v
                                }

createProject
    :: (MonadIO m, MonadMask m, MonadIO n, MonadMask n, MonadGit n)
    => (RepositoryUrl -> n () -> m ())
    -> PreliminaryProjectConfiguration
    -> Bool
    -> m ()
createProject withRepository projectConfiguration unattended =
    let finalProjectConfiguration =
                makeFinalProjectConfiguration projectConfiguration
    in
        withRepository
                (RepositoryUrl $ selectedTemplate finalProjectConfiguration)
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
