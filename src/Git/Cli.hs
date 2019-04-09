{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Cli where

import           Git                            ( RepositoryUrl(..)
                                                , GitRepository(..)
                                                , Committish(..)
                                                )
import           Logging
import           Data.Maybe
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Functor
import           Data.Foldable
import           Data.Either.Combinators
import           Path
import           Path.IO
import           System.Exit
import           System.Process.Typed

data GitException = GitCallException
    { exitCode :: Int
    , stdErr :: Text }
    deriving Show

instance Exception GitException

prefixed :: Doc () -> Text -> Doc ()
prefixed prefix =
    concatWith (\a b -> a <> hardline <> b)
        . map ((<>) prefix . pretty)
        . T.lines


decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8 . BS.toStrict

stdoutOrThrow
    :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ByteString
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow (ExitFailure code, out, err) =
    throwM $ GitCallException code (decodeUtf8 err)

stdoutOrNothing :: (ExitCode, ByteString, ByteString) -> Maybe ByteString
stdoutOrNothing (ExitSuccess     , out, err) = Just out
stdoutOrNothing (ExitFailure code, out, err) = Nothing

unitOrThrow :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ()
unitOrThrow a = void (stdoutOrThrow a)

throwIfFailed :: MonadThrow m => ExitCode -> m ()
throwIfFailed ExitSuccess = return ()
throwIfFailed (ExitFailure code) = throwM $ GitCallException code ""

gitCmd
        :: (MonadIO m, MonadThrow m, MonadLog m)
        => [Text]
        -> m (ExitCode, ByteString, ByteString)
gitCmd cmd = do
        logInfo $ "Running interactively git" <+> fillSep (map pretty cmd)
        let process = setStdin closed $ setStdout byteStringOutput $ setStderr byteStringOutput $ proc "git" (map T.unpack cmd) in do
            (exitCode, out, err) <- liftIO $ readProcess process
            logDebug $ prefixed "err> " (decodeUtf8 err)
            logDebug $ prefixed "out> " (decodeUtf8 out)
            return (exitCode, out, err)

gitRepoCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m (ExitCode, ByteString, ByteString)
gitRepoCmd (GitRepository path) cmd =
    gitCmd (map T.pack ["-C", toFilePath path] ++ cmd)

gitInteractiveCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => [Text]
    -> m ExitCode
gitInteractiveCmd cmd = do
    logInfo $ "Running git" <+> fillSep (map pretty cmd)
    let process = setStdin inherit $ setStdout inherit $ setStderr inherit $ proc "git" (map T.unpack cmd) in
        liftIO $ runProcess process

gitInteractiveRepoCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m ExitCode
gitInteractiveRepoCmd (GitRepository path) cmd =
    gitInteractiveCmd (map T.pack ["-C", toFilePath path] ++ cmd)

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
    withSystemTempDir "tenpureto" $ cloneReporitory url >=> f

withRepository :: Path Abs Dir -> (GitRepository -> m a) -> m a
withRepository path f = f (GitRepository path)

initRepository
    :: (MonadIO m, MonadMask m, MonadLog m) => Path Abs Dir -> m GitRepository
initRepository dir = do
    gitCmd ["init", T.pack (toFilePath dir)] >>= unitOrThrow
    return $ GitRepository dir

withNewWorktree
    :: (MonadIO m, MonadMask m, MonadLog m)
    => GitRepository
    -> Committish
    -> (GitRepository -> m a)
    -> m a
withNewWorktree repo c f =
    withSystemTempDir "tenpureto" (initWorktree repo c >=> f)
        `finally` pruneWorktrees repo

initWorktree
    :: (MonadIO m, MonadMask m, MonadLog m)
    => GitRepository
    -> Committish
    -> Path Abs Dir
    -> m GitRepository
initWorktree repo (Committish c) dir = do
    gitRepoCmd
            repo
            ["worktree", "add", "--no-checkout", T.pack (toFilePath dir), c]
        >>= unitOrThrow
    return $ GitRepository dir

pruneWorktrees :: (MonadIO m, MonadMask m, MonadLog m) => GitRepository -> m ()
pruneWorktrees repo = gitRepoCmd repo ["worktree", "prune"] >>= unitOrThrow

cloneReporitory
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => RepositoryUrl
    -> Path Abs Dir
    -> m GitRepository
cloneReporitory (RepositoryUrl url) dst =
    let repo = GitRepository dst
    in  do
            gitCmd ["init", T.pack (toFilePath dst)] >>= unitOrThrow
            gitRepoCmd repo ["remote", "add", "origin", url] >>= unitOrThrow
            gitRepoCmd repo ["fetch", "origin"] >>= unitOrThrow
            return repo

listBranches
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> m [Text]
listBranches repo prefix = T.lines . decodeUtf8 <$> gitcmdStdout
    repo
    [ "for-each-ref"
    , "--format=%(refname:strip=3)"
    , "refs/remotes/origin/" <> prefix
    ]

getBranchFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> Path Rel File
    -> m (Maybe ByteString)
getBranchFile repo branch file = stdoutOrNothing <$> gitRepoCmd repo ["show", branch <> ":" <> T.pack (toFilePath file)]

getWorkingCopyFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Path Rel File
    -> m (Maybe ByteString)
getWorkingCopyFile (GitRepository repoPath) file = do
    result <- liftIO $ try $ BS.readFile (toFilePath (repoPath </> file))
    return $ rightToMaybe (result :: Either SomeException ByteString)

checkoutBranch
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> Text
    -> m ()
checkoutBranch repo branch name = do
    gitRepoCmd repo ["checkout", branch] >>= unitOrThrow
    gitRepoCmd repo ["checkout", "-b", name] >>= unitOrThrow

listConflicts
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> m [Path Rel File]
listConflicts repo =
    gitRepoCmd repo ["diff", "--name-only", "--diff-filter=U"]
        >>= stdoutOrThrow
        >>= traverse (parseRelFile . T.unpack)
        .   (T.lines . decodeUtf8)

listStaged
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> m [Path Rel File]
listStaged repo =
    gitRepoCmd repo ["diff", "--cached", "--name-only"]
        >>= stdoutOrThrow
        >>= traverse (parseRelFile . T.unpack)
        .   (T.lines . decodeUtf8)

mergeBranch
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> ([Path Rel File] -> m ())
    -> m ()
mergeBranch repo branch resolve = do
    (mergeResult, _, _) <- gitRepoCmd
        repo
        ["merge", "--no-commit", "--no-ff", branch]
    case mergeResult of
        ExitSuccess      -> return ()
        ExitFailure code -> listConflicts repo >>= resolve

writeAddFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Path Rel File
    -> ByteString
    -> m ()
writeAddFile repo file content = do
    logInfo $ "Writing to " <+> pretty file
    logDebug $ prefixed "file> " (decodeUtf8 content)
    liftIO $ (BS.writeFile . toFilePath) (repositoryPath repo </> file) content
    addFiles repo [file]

addFiles
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Path Rel File]
    -> m ()
addFiles repo files =
    gitRepoCmd repo
               (["add", "--force", "--"] ++ map (T.pack . toFilePath) files)
        >>= unitOrThrow

runMergeTool :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> m ()
runMergeTool repo = gitInteractiveRepoCmd repo ["mergetool"] >>= throwIfFailed

outputToCommittish :: ByteString -> Maybe Committish
outputToCommittish =
    fmap Committish
        . mfilter (not . T.null)
        . listToMaybe
        . T.lines
        . decodeUtf8

commit
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> m (Maybe Committish)
commit repo message = do
    staged <- listStaged repo
    if null staged
        then return Nothing
        else do
            gitRepoCmd repo ["commit", "--message", message] >>= unitOrThrow
            gitcmdStdout repo ["rev-parse", "--verify", "HEAD"]
                <&> outputToCommittish

findCommit
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> m (Maybe Committish)
findCommit repo pat = gitcmdStdout repo ["rev-list", "--max-count=1", "--date-order", "--grep", pat, "HEAD"] <&> outputToCommittish
