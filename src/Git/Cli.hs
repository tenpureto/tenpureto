{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Cli where

import           Data.Maybe
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Functor
import           Data.Either.Combinators
import           Path
import           Path.IO
import           System.Exit

import           Tenpureto.Exec
import           Logging
import           Git                            ( RepositoryUrl(..)
                                                , GitRepository(..)
                                                , Committish(..)
                                                , BranchRef(..)
                                                , PushSpec(..)
                                                )

gitCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => [Text]
    -> m (ExitCode, ByteString, ByteString)
gitCmd cmd = runCmd ("git" :| cmd)

gitRepoCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m (ExitCode, ByteString, ByteString)
gitRepoCmd (GitRepository path) cmd =
    gitCmd (map T.pack ["-C", toFilePath path] ++ cmd)

gitInteractiveCmd
    :: (MonadIO m, MonadThrow m, MonadLog m) => [Text] -> m ExitCode
gitInteractiveCmd cmd = runInteractiveCmd ("git" :| cmd)

gitInteractiveRepoCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m ExitCode
gitInteractiveRepoCmd (GitRepository path) cmd =
    gitInteractiveCmd (map T.pack ["-C", toFilePath path] ++ cmd)

gitCmdStdout
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m ByteString
gitCmdStdout repo cmd = gitRepoCmd repo cmd >>= stdoutOrThrow

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
    let repo = GitRepository dir
    gitRepoCmd repo ["config", "rerere.enabled", "true"] >>= unitOrThrow
    return repo

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
            gitRepoCmd repo ["config", "rerere.enabled", "true"] >>= unitOrThrow
            gitRepoCmd repo ["remote", "add", "origin", url] >>= unitOrThrow
            gitRepoCmd repo ["fetch", "origin"] >>= unitOrThrow
            return repo

listBranches
    :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> m [Text]
listBranches repo = T.lines . decodeUtf8 <$> gitCmdStdout
    repo
    ["for-each-ref", "--format=%(refname:strip=3)", "refs/remotes/origin/"]

getRepositoryFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Committish
    -> Path Rel File
    -> m (Maybe ByteString)
getRepositoryFile repo (Committish c) file = maybeStdout
    <$> gitRepoCmd repo ["show", c <> ":" <> T.pack (toFilePath file)]

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
    -> Maybe Text
    -> m ()
checkoutBranch repo branch name = do
    gitRepoCmd repo ["checkout", branch] >>= unitOrThrow
    gitRepoCmd
            repo
            (maybe ["checkout", "--detach"] (\n -> ["checkout", "-b", n]) name)
        >>= unitOrThrow

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
        ExitSuccess   -> return ()
        ExitFailure _ -> listConflicts repo >>= resolve

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
            gitCmdStdout repo ["rev-parse", "--verify", "HEAD"]
                <&> outputToCommittish

findCommitByRef
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> BranchRef
    -> m (Maybe Committish)
findCommitByRef repo (BranchRef branch) =
    gitCmdStdout repo ["rev-parse", "--verify", branch] <&> outputToCommittish

findCommitByMessage
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> m (Maybe Committish)
findCommitByMessage repo pat =
    gitCmdStdout
            repo
            ["rev-list", "--max-count=1", "--date-order", "--grep", pat, "HEAD"]
        <&> outputToCommittish

getCommitMessage
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Committish
    -> m Text
getCommitMessage repo (Committish c) =
    gitCmdStdout repo ["rev-list", "--max-count=1", "--format=%B", c]
        <&> decodeUtf8

gitDiffHasCommits
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Committish
    -> Committish
    -> m Bool
gitDiffHasCommits repo (Committish c) (Committish base) =
    gitCmdStdout repo ["rev-list", "--count", c, "^" <> base]
        <&> decodeUtf8
        <&> T.strip
        <&> ("0" /=)

gitLogDiff
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Committish
    -> Committish
    -> m Text
gitLogDiff repo (Committish c) (Committish base) =
    gitCmdStdout repo ["log", "--patch", "--color", c, "^" <> base]
        <&> decodeUtf8

listFiles
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> m [Path Rel File]
listFiles repo =
    gitCmdStdout repo ["ls-files"]
        >>= traverse (parseRelFile . T.unpack)
        .   (T.lines . decodeUtf8)

populateRerereFromMerge
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Committish
    -> m ()
populateRerereFromMerge repo (Committish c) = do
    parents <- T.words . decodeUtf8 <$> gitCmdStdout
        repo
        ["rev-list", "--max-count=1", "--parents", c]
    rerunMerge parents
  where
    rerunMerge :: (MonadIO m, MonadThrow m, MonadLog m) => [Text] -> m ()
    rerunMerge [mc, p1, p2] = do
        gitRepoCmd repo ["checkout", p1] >>= unitOrThrow
        (mergeResult, _, _) <- gitRepoCmd repo ["merge", "--no-commit", p2]
        case mergeResult of
            ExitSuccess   -> return ()
            ExitFailure _ -> do
                gitRepoCmd repo ["rerere"] >>= unitOrThrow
                gitRepoCmd repo ["checkout", mc, "--", "."] >>= unitOrThrow
                gitRepoCmd repo ["rerere"] >>= unitOrThrow
                gitRepoCmd repo ["reset", "--hard"] >>= unitOrThrow
    rerunMerge _ = return ()

getCurrentBranch
    :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> m Text
getCurrentBranch repo = T.strip . decodeUtf8 <$> gitCmdStdout
    repo
    ["rev-parse", "--abbrev-ref", "HEAD"]

getCurrentHead
    :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> m Committish
getCurrentHead repo = Committish . T.strip . decodeUtf8 <$> gitCmdStdout
    repo
    ["rev-parse", "HEAD"]

renameCurrentBranch
    :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> Text -> m ()
renameCurrentBranch repo name =
    gitRepoCmd repo ["branch", "--move", name] >>= unitOrThrow

pushRefs
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [PushSpec]
    -> m ()
pushRefs repo refs =
    gitRepoCmd repo (["push", "--atomic", "origin"] ++ fmap refspec refs)
        >>= unitOrThrow
  where
    ref (BranchRef dst) = "refs/heads/" <> dst
    refspec CreateBranch { sourceCommit = (Committish c), destinationRef = dst }
        = c <> ":" <> ref dst
    refspec UpdateBranch { sourceCommit = (Committish c), destinationRef = dst }
        = c <> ":" <> ref dst
    refspec DeleteBranch { destinationRef = dst } = ":" <> ref dst
