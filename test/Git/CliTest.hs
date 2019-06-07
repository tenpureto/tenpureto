{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Git.CliTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed
import           Path
import           Path.IO
import           Data.Foldable

import           Tenpureto.Exec
import           Git                            ( GitRepository(..)
                                                , Committish(..)
                                                )
import           Git.Cli
import           Logging

withTempRepository
    :: (MonadIO m, MonadMask m, MonadLog m) => (GitRepository -> m a) -> m a
withTempRepository f = withSystemTempDir "tenpureto"
    $ \dir -> withCurrentDir dir $ (initRepository >=> f) dir

data CmdException = CmdException { exitCode :: Int, stdOut :: Text, stdErr :: Text } deriving Show
instance Exception CmdException

testCmd :: (MonadIO m, MonadThrow m, MonadLog m) => Text -> m ()
testCmd cmd = do
    logInfo $ "Running " <+> pretty cmd
    (code, out, err) <- (readProcess . setStdin closed . shell) (T.unpack cmd)
    logDebug $ prefixed "err> " (decodeUtf8 err)
    logDebug $ prefixed "out> " (decodeUtf8 out)
    case code of
        ExitSuccess -> return ()
        ExitFailure c ->
            throwM $ CmdException c (decodeUtf8 out) (decodeUtf8 err)

testCmds :: (MonadIO m, MonadThrow m, MonadLog m) => [Text] -> m ()
testCmds = traverse_ testCmd

readCmd :: MonadIO m => GitRepository -> String -> m Text
readCmd (GitRepository dir) cmd =
    liftIO
        $   decodeUtf8
        <$> (readProcessStdout_ . setWorkingDir (toFilePath dir) . shell) cmd

test_populateRerereFromMerge :: IO [TestTree]
test_populateRerereFromMerge = discardLogging $ sequence
    [ withTempRepository $ \repo -> do
        testCmds
            [ "git commit --allow-empty -m base"
            , "git commit --allow-empty -m commit2"
            ]
        populateRerereFromMerge repo (Committish "HEAD")
        rerere <- listDir [reldir|.git/rr-cache|]
        return
            $   testCase "should ignore non-merge commits"
            $   rerere
            @?= ([], [])
    , withTempRepository $ \repo -> do
        testCmds initMergeConflict
        populateRerereFromMerge repo (Committish "HEAD")
        rerere <- listDir [reldir|.git/rr-cache|]
        return
            $   testCase "should fill rerere cache from a merge commit"
            $   length (fst rerere)
            @?= 1
    , withTempRepository $ \repo -> do
        testCmds initMergeConflict
        populateRerereFromMerge repo (Committish "HEAD")
        testCmds ["git checkout branch1", "git merge branch2 || true"]
        content <- liftIO $ readFile "file"
        return
            $   testCase "should allow resolution reuse"
            $   content
            @?= "version3\n"
    ]
  where
    initMergeConflict =
        [ "git config rerere.enabled false"
        , "git commit --allow-empty -m base"
        , "git checkout -b branch1"
        , "echo version1 > file"
        , "git add file"
        , "git commit -m commit1"
        , "git checkout master -b branch2"
        , "echo version2 > file"
        , "git add file"
        , "git commit -m commit2"
        , "git checkout -b merge"
        , "git merge --no-commit branch1 || true"
        , "echo version3 > file"
        , "git add file"
        , "git commit -m merge"
        , "git config rerere.enabled true"
        ]
