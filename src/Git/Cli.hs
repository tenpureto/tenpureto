{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.Cli where

import           Git
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Process

data GitException = GitCallException
    { exitCode :: Int
    , stdErr :: String }
    deriving Show

instance Exception GitException

withClonedRepository
    :: (MonadIO m, MonadMask m)
    => RepositoryUrl
    -> (GitRepository -> m a)
    -> m a
withClonedRepository url f =
    withSystemTempDirectory "tenpureto" $ cloneReporitory url >=> f

cloneReporitory :: MonadIO m => RepositoryUrl -> FilePath -> m GitRepository
cloneReporitory (RepositoryUrl url) dst = do
    liftIO $ callProcess "git" ["clone", "--no-checkout", url, dst]
    return $ GitRepository dst

stdoutOrThrow :: MonadThrow m => (ExitCode, String, String) -> m String
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow ((ExitFailure code), out, err) =
    throwM $ GitCallException code err

gitcmdStdout
    :: (MonadIO m, MonadThrow m) => GitRepository -> [String] -> m String
gitcmdStdout (GitRepository path) cmd =
    liftIO
        $   readProcessWithExitCode "git" (["-C", path] ++ cmd) ""
        >>= stdoutOrThrow

listBranches
    :: (MonadIO m, MonadThrow m) => GitRepository -> String -> m [String]
listBranches repo prefix = lines <$> gitcmdStdout
    repo
    [ "for-each-ref"
    , "--format=%(refname:strip=3)"
    , "refs/remotes/origin/" ++ prefix
    ]
