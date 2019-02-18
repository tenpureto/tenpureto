{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Git where

import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO
import           System.Exit
import           System.Process

newtype RepositoryUrl = RepositoryUrl String

class Monad m => MonadGit m where
    cloneRemoteReporitory :: RepositoryUrl -> FilePath -> m ()
    listBranches :: FilePath -> m [String]

newtype GitCLI a = GitCLI (IO a)
     deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

data GitException = GitCallException
    { exitCode :: Int
    , stdErr :: String }
    deriving Show

instance Exception GitException

runGitCLI :: GitCLI a -> IO a
runGitCLI (GitCLI a) = a

gitcmd :: [String] -> IO ()
gitcmd = callProcess "git"

stdoutOrThrow :: MonadThrow m => (ExitCode, String, String) -> m String
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow ((ExitFailure code), out, err) =
    throwM $ GitCallException code err

gitcmdStdout :: (MonadIO m, MonadThrow m) => [String] -> m String
gitcmdStdout cmd =
    liftIO $ readProcessWithExitCode "git" cmd "" >>= stdoutOrThrow

instance MonadGit GitCLI where
    cloneRemoteReporitory (RepositoryUrl url) dst =
        liftIO $ gitcmd ["clone", "--no-checkout", url, dst]
    listBranches repo = fmap lines $ liftIO $ gitcmdStdout
        [ "-C"
        , repo
        , "for-each-ref"
        , "--format='%(refname:strip=3)'"
        , "refs/remotes/origin"
        ]
