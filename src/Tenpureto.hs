module Tenpureto where

import           Git

createProject :: MonadGit m => String -> Bool -> m ()
createProject template unattended = 
    cloneRemoteReporitory
        (RepositoryUrl template)
        (RepositoryLocation "/tmp/test")

updateProject :: MonadGit m => Maybe String -> Bool -> m ()
updateProject template unattended = 
    return ()
