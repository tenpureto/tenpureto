{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Git where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , runReaderT
                                                )
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Process

newtype RepositoryUrl = RepositoryUrl String

class Monad m => MonadGit m where
    listBranches :: String -> m [String]

newtype CliRepo = CliRepo FilePath
type CliRepoT = ReaderT CliRepo

data GitException = GitCallException
    { exitCode :: Int
    , stdErr :: String }
    deriving Show

instance Exception GitException

withClonedRepository
    :: (MonadIO m, MonadMask m) => RepositoryUrl -> CliRepoT m a -> m a
withClonedRepository url f = withSystemTempDirectory "tenpureto"
    $ \x -> cloneReporitory url x >> runReaderT f (CliRepo x)

cloneReporitory :: (MonadIO m) => RepositoryUrl -> FilePath -> m ()
cloneReporitory (RepositoryUrl url) dst =
    liftIO $ callProcess "git" ["clone", "--no-checkout", url, dst]

stdoutOrThrow :: MonadThrow m => (ExitCode, String, String) -> m String
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow ((ExitFailure code), out, err) =
    throwM $ GitCallException code err

gitcmdStdout
    :: (MonadIO m, MonadThrow m, MonadReader CliRepo m) => [String] -> m String
gitcmdStdout cmd = do
    CliRepo repo <- ask
    liftIO
        $   readProcessWithExitCode "git" (["-C", repo] ++ cmd) ""
        >>= stdoutOrThrow

instance (MonadMask m, MonadIO m) => MonadGit (CliRepoT m) where

    listBranches prefix = lines <$> gitcmdStdout
        [ "for-each-ref"
        , "--format=%(refname:strip=3)"
        , "refs/remotes/origin/" ++ prefix
        ]
