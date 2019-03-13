{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Cli where

import           Git
import           Logging
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

import qualified Control.Monad.Log             as L

data GitException = GitCallException
    { exitCode :: Int
    , stdErr :: Text }
    deriving Show

instance Exception GitException

stdoutOrThrow
    :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ByteString
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow (ExitFailure code, out, err) =
    throwM $ GitCallException code (E.decodeUtf8 err)

stdoutOrNothing :: (ExitCode, ByteString, ByteString) -> Maybe ByteString
stdoutOrNothing (ExitSuccess     , out, err) = Just out
stdoutOrNothing (ExitFailure code, out, err) = Nothing

unitOrThrow :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ()
unitOrThrow a = const () <$> stdoutOrThrow a

gitCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => [Text]
    -> m (ExitCode, ByteString, ByteString)
gitCmd cmd = do
    logInfo $ "Running git" <+> fillSep (map pretty cmd)
    (exitCode, out, err) <- liftIO
        $ BP.readProcessWithExitCode "git" (map T.unpack cmd) BS.empty
    logDebug $ (align . pretty) (E.decodeUtf8 err)
    logDebug $ (align . pretty) (E.decodeUtf8 out)
    return (exitCode, out, err)

gitRepoCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m (ExitCode, ByteString, ByteString)
gitRepoCmd (GitRepository path) cmd = gitCmd (map T.pack ["-C", path] ++ cmd)

gitcmdStdout
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m ByteString
gitcmdStdout repo cmd = gitRepoCmd repo cmd >>= stdoutOrThrow


withClonedRepository
    :: (MonadIO m, MonadMask m, MonadLog m)
    => RepositoryUrl
    -> (GitRepository -> m a)
    -> m a
withClonedRepository url f =
    withSystemTempDirectory "tenpureto" $ cloneReporitory url >=> f

cloneReporitory
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => RepositoryUrl
    -> FilePath
    -> m GitRepository
cloneReporitory (RepositoryUrl url) dst = do
    (exitCode, out, err) <- gitCmd ["clone", "--no-checkout", url, T.pack dst]
    case exitCode of
        ExitSuccess      -> return $ GitRepository dst
        ExitFailure code -> throwM $ GitCallException code (E.decodeUtf8 err)

listBranches
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> m [Text]
listBranches repo prefix = T.lines . E.decodeUtf8 <$> gitcmdStdout
    repo
    [ "for-each-ref"
    , "--format=%(refname:strip=3)"
    , "refs/remotes/origin/" <> prefix
    ]

getBranchFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> Text
    -> m (Maybe ByteString)
getBranchFile repo branch file =
    stdoutOrNothing <$> gitRepoCmd repo ["show", branch <> ":" <> file]

checkoutBranch
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> Text
    -> m ()
checkoutBranch repo branch name = do
    gitRepoCmd repo ["checkout", branch] >>= unitOrThrow
    gitRepoCmd repo ["checkout", "-b", name] >>= unitOrThrow

mergeBranch
    :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> Text -> m ()
mergeBranch repo branch =
    gitRepoCmd repo ["merge", "--no-ff", branch] >>= unitOrThrow
