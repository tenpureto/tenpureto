{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Git where

import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO
import           System.Exit
import           System.Process

newtype RepositoryUrl = RepositoryUrl String
newtype RepositoryLocation = RepositoryLocation String

class Monad m => MonadGit m where
    cloneRemoteReporitory :: RepositoryUrl -> RepositoryLocation -> m ()


newtype GitCLI a = GitCLI (IO a)
     deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runGitCLI :: GitCLI a -> IO a
runGitCLI (GitCLI a) = a

gitcmd :: [String] -> IO ()
gitcmd = callProcess "git"

instance MonadGit GitCLI where
    cloneRemoteReporitory (RepositoryUrl url) (RepositoryLocation dst) =
        liftIO $ gitcmd ["clone", "--no-checkout", url, dst]
