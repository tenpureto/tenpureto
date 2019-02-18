module Tenpureto where

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

createProject :: (MonadGit m, MonadIO m, MonadMask m) => String -> Bool -> m ()
createProject template unattended =
    withSystemTempDirectory "tenpureto" $ prepareTemplate template unattended

updateProject
    :: (MonadGit m, MonadIO m, MonadMask m) => Maybe String -> Bool -> m ()
updateProject template unattended = return ()

prepareTemplate
    :: (MonadGit m, MonadIO m, MonadMask m)
    => String
    -> Bool
    -> FilePath
    -> m ()
prepareTemplate template unattended path = do
    cloneRemoteReporitory (RepositoryUrl template) path
    branches <- listBranches path
    liftIO $ putStrLn $ intercalate ", " branches
