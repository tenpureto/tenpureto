{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Cli where

import           Git
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding            as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.IO
import           System.IO.Temp
import           System.Exit
import           System.Process
import qualified System.Process.ByteString     as BP

data GitException = GitCallException
    { exitCode :: Int
    , stdErr :: Text }
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

stdoutOrThrow
    :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ByteString
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow (ExitFailure code, out, err) =
    throwM $ GitCallException code (E.decodeUtf8 err)

stdoutOrNothing :: (ExitCode, ByteString, ByteString) -> Maybe ByteString
stdoutOrNothing (ExitSuccess     , out, err) = Just out
stdoutOrNothing (ExitFailure code, out, err) = Nothing

gitcmd
    :: (MonadIO m, MonadThrow m)
    => GitRepository
    -> [String]
    -> m (ExitCode, ByteString, ByteString)
gitcmd (GitRepository path) cmd =
    liftIO $ BP.readProcessWithExitCode "git" (["-C", path] ++ cmd) BS.empty

gitcmdStdout
    :: (MonadIO m, MonadThrow m) => GitRepository -> [String] -> m ByteString
gitcmdStdout repo cmd = gitcmd repo cmd >>= stdoutOrThrow

listBranches
    :: (MonadIO m, MonadThrow m) => GitRepository -> String -> m [String]
listBranches repo prefix =
    map T.unpack . T.lines . E.decodeUtf8 <$> gitcmdStdout
        repo
        [ "for-each-ref"
        , "--format=%(refname:strip=3)"
        , "refs/remotes/origin/" ++ prefix
        ]

getBranchFile
    :: (MonadIO m, MonadThrow m)
    => GitRepository
    -> String
    -> String
    -> m (Maybe ByteString)
getBranchFile repo branch file =
    stdoutOrNothing <$> gitcmd repo ["show", branch ++ ":" ++ file]
