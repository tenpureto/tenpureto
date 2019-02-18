module Tenpureto
    ( createProject
    , updateProject
    , withClonedRepository
    )
where

import           Data.List
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO.Temp
import           Git

data TemplateBranchConfiguration = TemplateBranchConfiguration
    { branchName :: String
    , requiredBranches :: [String]
    , branchVariables :: [String]
    }

data ProjectConfiguration = ProjectConfiguration
    { selectedBranches :: [String]
    , variableValues :: [(String, String)]
    }

createProject
    :: (MonadIO m, MonadMask m, MonadIO n, MonadMask n, MonadGit n)
    => (RepositoryUrl -> n () -> m ())
    -> String
    -> Bool
    -> m ()
createProject withRepository template unattended =
    withRepository (RepositoryUrl template)
        $ prepareTemplate template unattended

updateProject
    :: (MonadIO m, MonadMask m, MonadIO n, MonadMask n, MonadGit n)
    => (RepositoryUrl -> n () -> m ())
    -> Maybe String
    -> Bool
    -> m ()
updateProject withRepository template unattended = return ()

prepareTemplate
    :: (MonadIO m, MonadMask m, MonadGit m) => String -> Bool -> m ()
prepareTemplate template unattended = do
    branches <- listBranches
    liftIO $ putStrLn $ intercalate ", " branches
