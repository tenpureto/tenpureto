{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Cli where

import           Git                            ( RepositoryUrl(..)
                                                , GitRepository(..)
                                                )
import           Logging
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding            as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Foldable
import           Path
import           Path.IO
import           System.Exit
import           System.Process
import qualified System.Process.ByteString     as BP

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

stdoutOrThrow
    :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ByteString
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow (ExitFailure code, out, err) =
    throwM $ GitCallException code (E.decodeUtf8 err)

stdoutOrNothing :: (ExitCode, ByteString, ByteString) -> Maybe ByteString
stdoutOrNothing (ExitSuccess     , out, err) = Just out
stdoutOrNothing (ExitFailure code, out, err) = Nothing

unitOrThrow :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ()
unitOrThrow a = void (stdoutOrThrow a)

gitCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => [Text]
    -> m (ExitCode, ByteString, ByteString)
gitCmd cmd = do
    logInfo $ "Running git" <+> fillSep (map pretty cmd)
    (exitCode, out, err) <- liftIO
        $ BP.readProcessWithExitCode "git" (map T.unpack cmd) BS.empty
    logDebug $ prefixed "err> " (E.decodeUtf8 err)
    logDebug $ prefixed "out> " (E.decodeUtf8 out)
    return (exitCode, out, err)

gitRepoCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> [Text]
    -> m (ExitCode, ByteString, ByteString)
gitRepoCmd (GitRepository path) cmd =
    gitCmd (map T.pack ["-C", toFilePath path] ++ cmd)

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

initRepository
    :: (MonadIO m, MonadMask m, MonadLog m) => Path Abs Dir -> m GitRepository
initRepository dir =
    GitRepository
        <$> (gitCmd ["init", T.pack (toFilePath dir)] >>= unitOrThrow >> liftIO
                (makeAbsolute dir)
            )

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
    -> Path Rel File
    -> m (Maybe ByteString)
getBranchFile repo branch file = stdoutOrNothing
    <$> gitRepoCmd repo ["show", branch <> ":" <> T.pack (toFilePath file)]

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
        .   (T.lines . E.decodeUtf8)

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
    commit repo ("Merge " <> branch)

writeAddFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Path Rel File
    -> ByteString
    -> m ()
writeAddFile repo file content = do
    logInfo $ "Writing to " <+> pretty file
    logDebug $ prefixed "file> " (E.decodeUtf8 content)
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
runMergeTool repo = gitRepoCmd repo ["mergetool"] >>= unitOrThrow

commit :: (MonadIO m, MonadThrow m, MonadLog m) => GitRepository -> Text -> m ()
commit repo message =
    gitRepoCmd repo ["commit", "--message", message] >>= unitOrThrow
