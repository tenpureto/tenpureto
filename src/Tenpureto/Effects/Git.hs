{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.Git
    ( module Tenpureto.Effects.Git
    , Text
    , Committish(..)
    , GitRepository(..)
    , GitException
    , Path
    , Abs
    , Rel
    , Dir
    , File
    )
where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Resource

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Functor
import           Data.List

import           Path

import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Process
import           Tenpureto.Effects.Git.Internal

newtype RepositoryUrl = RepositoryUrl Text deriving (Eq, Show)

newtype BranchRef = BranchRef { reference :: Text } deriving (Show)

data PushSpec = CreateBranch { sourceCommit :: Committish, destinationRef :: BranchRef }
              | UpdateBranch { sourceCommit :: Committish, destinationRef :: BranchRef, pullRequestRef :: BranchRef, pullRequestTitle :: Text }
              | DeleteBranch { destinationRef :: BranchRef }
              deriving (Show)

data PullRequestSettings = PullRequestSettings { pullRequestAddLabels :: [Text], pullRequestAssignTo :: [Text] }

data MergeResult = MergeSuccess
                 | MergeConflicts [Path Rel File]

data Git m a where
    InitRepository ::Path Abs Dir -> Git m GitRepository
    AddRepositoryOrigin ::GitRepository -> RepositoryUrl -> Git m ()
    InitWorktree ::GitRepository -> Committish -> Path Abs Dir -> Git m GitRepository
    DeleteWorktree ::GitRepository -> Path Abs Dir -> Git m ()
    ListBranches ::GitRepository -> Git m [Text]
    CheckoutBranch ::GitRepository -> Text -> Maybe Text -> Git m ()
    MergeBranch ::GitRepository -> Text -> Git m MergeResult
    RunMergeTool ::GitRepository -> Git m ()
    GetRepositoryFile ::GitRepository -> Committish -> Path Rel File -> Git m (Maybe ByteString)
    GetWorkingCopyFile ::GitRepository -> Path Rel File -> Git m (Maybe ByteString)
    WriteRepoFile ::GitRepository -> Path Rel File -> ByteString -> Git m ()
    AddFiles ::GitRepository -> [Path Rel File] -> Git m ()
    Commit ::GitRepository -> Text -> Git m (Maybe Committish)
    FindCommitByRef ::GitRepository -> BranchRef -> Git m (Maybe Committish)
    FindCommitByMessage ::GitRepository -> Text -> Git m (Maybe Committish)
    GetCommitMessage ::GitRepository -> Committish -> Git m Text
    GitDiffHasCommits ::GitRepository -> Committish -> Committish -> Git m Bool
    GitLogDiff ::GitRepository -> Committish -> Committish -> Git m Text
    ListFiles ::GitRepository -> Git m [Path Rel File]
    GetCurrentBranch ::GitRepository -> Git m Text
    GetCurrentHead ::GitRepository -> Git m Committish
    RenameCurrentBranch ::GitRepository -> Text -> Git m ()
    PushRefs ::GitRepository -> [PushSpec] -> Git m ()

makeSem ''Git

data GitServer m a where
    CreateOrUpdatePullRequest ::GitRepository -> PullRequestSettings -> Committish -> Text -> Text -> Text -> GitServer m ()

makeSem ''GitServer


writeAddFile
    :: Member Git r => GitRepository -> Path Rel File -> ByteString -> Sem r ()
writeAddFile repo file content = do
    writeRepoFile repo file content
    addFiles repo [file]

withRepository
    :: Member Git r => Path Abs Dir -> (GitRepository -> Sem r a) -> Sem r a
withRepository path f = f (GitRepository path)

withClonedRepository
    :: Members '[Resource, FileSystem, Git] r
    => RepositoryUrl
    -> (GitRepository -> Sem r a)
    -> Sem r a
withClonedRepository url f = withSystemTempDir "tenpureto" $ \dir -> do
    repo <- initRepository dir
    addRepositoryOrigin repo url
    f repo

withNewWorktree
    :: Members '[Resource, FileSystem, Git] r
    => GitRepository
    -> Committish
    -> (GitRepository -> Sem r a)
    -> Sem r a
withNewWorktree repo c f = withSystemTempDir "tenpureto" $ \dir ->
    bracket (initWorktree repo c dir) (const $ deleteWorktree repo dir) f

runGit
    :: Members '[FileSystem, Process, Error GitException] r
    => Sem (Git ': r) a
    -> Sem r a
runGit = interpret $ \case

    InitRepository dir -> do
        ensureEmptyDir dir
        gitCmd ["init", T.pack (toFilePath dir)] >>= asUnit
        return $ GitRepository dir

    AddRepositoryOrigin repo (RepositoryUrl url) -> do
        gitRepoCmd repo ["remote", "add", "origin", url] >>= asUnit
        gitRepoCmd repo ["fetch", "origin"] >>= asUnit

    InitWorktree repo (Committish c) dir -> do
        gitRepoCmd
                repo
                ["worktree", "add", "--no-checkout", T.pack (toFilePath dir), c]
            >>= asUnit
        return $ GitRepository dir

    DeleteWorktree repo dir -> do
        -- "git worktree remove" required too modern git
        removeDirRecur dir
        gitRepoCmd repo ["worktree", "prune"] >>= asUnit

    ListBranches repo ->
        gitRepoCmd
                repo
                [ "for-each-ref"
                , "--format=%(refname:strip=3)"
                , "refs/remotes/origin/"
                ]
            >>= asLines

    CheckoutBranch repo branch name -> do
        gitRepoCmd repo ["checkout", branch] >>= asUnit
        gitRepoCmd repo (asName name) >>= asUnit
      where
        asName Nothing  = ["checkout", "--detach"]
        asName (Just n) = ["checkout", "-b", n]

    MergeBranch repo branch -> do
        (mergeResult, _, _) <- gitRepoCmd
            repo
            ["merge", "--no-commit", "--no-ff", branch]
        case mergeResult of
            ExitSuccess -> return MergeSuccess
            ExitFailure _ ->
                gitRepoCmd repo ["diff", "--name-only", "--diff-filter=U"]
                    >>= asFiles
                    <&> MergeConflicts

    RunMergeTool repo -> gitInteractiveRepoCmd repo ["mergetool"]

    GetRepositoryFile repo (Committish c) file ->
        gitRepoCmd repo ["show", c <> ":" <> T.pack (toFilePath file)]
            <&> maybeByteString

    GetWorkingCopyFile repo file ->
        readFileAsMaybeByteString (repositoryPath repo </> file)
            <&> fmap BS.fromStrict

    WriteRepoFile repo file content -> writeFileAsByteString
        (repositoryPath repo </> file)
        (BS.toStrict content)

    AddFiles repo files ->
        gitRepoCmd
                repo
                (["add", "--force", "--"] ++ map (T.pack . toFilePath) files)
            >>= asUnit

    Commit repo message -> do
        staged <-
            gitRepoCmd repo ["diff", "--cached", "--name-only"] >>= asFiles
        if null staged
            then return Nothing
            else do
                gitRepoCmd repo ["commit", "--message", message] >>= asUnit
                gitRepoCmd repo ["rev-parse", "--verify", "HEAD"]
                    >>= asCommittish
                    <&> Just

    FindCommitByRef repo (BranchRef branch) ->
        gitRepoCmd repo ["rev-parse", "--verify", branch] >>= asMaybeCommittish

    FindCommitByMessage repo pat ->
        gitRepoCmd
                repo
                [ "rev-list"
                , "--max-count=1"
                , "--date-order"
                , "--grep"
                , pat
                , "HEAD"
                ]
            >>= asMaybeCommittish

    GetCommitMessage repo (Committish c) ->
        gitRepoCmd repo ["rev-list", "--max-count=1", "--format=%B", c]
            >>= asText

    GitDiffHasCommits repo (Committish c) (Committish base) ->
        gitRepoCmd repo ["rev-list", "--count", c, "^" <> base]
            >>= asText
            <&> T.strip
            <&> ("0" /=)

    GitLogDiff repo (Committish c) (Committish base) ->
        gitRepoCmd repo ["log", "--patch", "--color", c, "^" <> base] >>= asText

    ListFiles repo -> gitRepoCmd repo ["ls-files"] >>= asFiles

    GetCurrentBranch repo ->
        gitRepoCmd repo ["rev-parse", "--abbrev-ref", "HEAD"]
            >>= asText
            <&> T.strip

    GetCurrentHead repo ->
        gitRepoCmd repo ["rev-parse", "HEAD"] >>= asCommittish

    RenameCurrentBranch repo name ->
        gitRepoCmd repo ["branch", "--move", name] >>= asUnit

    PushRefs repo refs ->
        gitRepoCmd repo (["push", "--atomic", "origin"] ++ fmap refspec refs)
            >>= asUnit
      where
        ref (BranchRef dst) = "refs/heads/" <> dst
        refspec CreateBranch { sourceCommit = (Committish c), destinationRef = dst }
            = c <> ":" <> ref dst
        refspec UpdateBranch { sourceCommit = (Committish c), destinationRef = dst }
            = c <> ":" <> ref dst
        refspec DeleteBranch { destinationRef = dst } = ":" <> ref dst


runGitHub
    :: Members '[Process, Error GitException] r
    => Sem (GitServer ': r) a
    -> Sem r a
runGitHub = interpret $ \case

    CreateOrUpdatePullRequest repo settings (Committish c) title source target
        -> do
            createOrUpdateReference repo (Committish c) source
            exitingPullRequests <-
                hubApiGetCmd repo
                             "/repos/{owner}/{repo}/pulls"
                             [("head", source), ("base", target)]
                    >>= asApiResponse
            pullRequest <- case exitingPullRequests of
                pullRequest : _ -> return pullRequest
                [] ->
                    hubApiCmd
                            repo
                            ApiPost
                            "/repos/{owner}/{repo}/pulls"
                            PullRequestInputPayload
                                { pullRequestHead     = source
                                , pullRequestBase     = target
                                , setPullRequestTitle = Just title
                                }
                        >>= asApiResponse
            hubApiCmd
                    repo
                    ApiPatch
                    (  "/repos/{owner}/{repo}/issues/"
                    <> (T.pack . show . pullRequestNumber) pullRequest
                    )
                    IssueInputPayload
                        { setIssueAssignees = nub
                                              $  fmap
                                                     assigneeLogin
                                                     (pullRequestAssignees
                                                         pullRequest
                                                     )
                                              ++ pullRequestAssignTo settings
                        , setIssueLabels    = nub
                                              $  fmap
                                                     labelName
                                                     (pullRequestLabels pullRequest)
                                              ++ pullRequestAddLabels settings
                        }
                >>= asUnit
