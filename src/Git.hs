{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Git where

import           Control.Monad.IO.Class

newtype RepositoryUrl = RepositoryUrl String
newtype RepositoryLocation = RepositoryLocation String

class Monad m => MonadGit m where
    cloneRemoteReporitory :: RepositoryUrl -> RepositoryLocation -> m ()


newtype GitCLI a = GitCLI (IO a)
     deriving (Functor, Applicative, Monad, MonadIO)

runGitCLI :: GitCLI a -> IO a
runGitCLI (GitCLI a) = a
     
instance MonadGit GitCLI where
    cloneRemoteReporitory (RepositoryUrl url) (RepositoryLocation dst) =
        liftIO $ putStrLn ("Cloning " ++ url)
