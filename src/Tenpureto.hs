module Tenpureto where

import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO.Temp
import           Git

createProject :: (MonadGit m, MonadIO m, MonadMask m) => String -> Bool -> m ()
createProject template unattended =
    withSystemTempDirectory "tenpureto" $ prepareTemplate template unattended

updateProject :: (MonadGit m, MonadIO m, MonadMask m) => Maybe String -> Bool -> m ()
updateProject template unattended = return ()

prepareTemplate
    :: (MonadGit m, MonadMask m) => String -> Bool -> FilePath -> m ()
prepareTemplate template unattended path =
    cloneRemoteReporitory (RepositoryUrl template) (RepositoryLocation path)
