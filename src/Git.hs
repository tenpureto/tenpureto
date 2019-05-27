module Git where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text )
import           Path
import           Logging

newtype RepositoryUrl = RepositoryUrl Text deriving (Eq, Show)
newtype GitRepository = GitRepository { repositoryPath :: Path Abs Dir }
newtype Committish = Committish Text deriving (Eq, Show)

newtype BranchRef = BranchRef { reference :: Text } deriving (Show)
data PushSpec = CreateBranch { sourceCommit :: Committish, destinationRef :: BranchRef }
              | UpdateBranch { sourceCommit :: Committish, destinationRef :: BranchRef, pullRequestRef :: BranchRef, pullRequestTitle :: Text }
              | DeleteBranch { destinationRef :: BranchRef }
              deriving (Show)

data PullRequestSettings = PullRequestSettings { pullRequestAddLabels :: [Text], pullRequestAssignTo :: [Text] }

class Monad m => MonadGit m where
    withClonedRepository :: RepositoryUrl -> (GitRepository -> m a) -> m a
    withNewWorktree :: GitRepository -> Committish -> (GitRepository -> m a) -> m a
    withRepository :: Path Abs Dir -> (GitRepository -> m a) -> m a
    initRepository :: Path Abs Dir -> m GitRepository
    listBranches :: GitRepository -> m [Text]
    checkoutBranch :: GitRepository -> Text -> Maybe Text -> m ()
    mergeBranch :: GitRepository -> Text -> ([Path Rel File] -> m ()) -> m ()
    runMergeTool :: GitRepository -> m ()
    getRepositoryFile :: GitRepository -> Committish -> Path Rel File -> m (Maybe ByteString)
    getWorkingCopyFile :: GitRepository -> Path Rel File -> m (Maybe ByteString)
    writeAddFile :: GitRepository -> Path Rel File -> ByteString -> m ()
    addFiles :: GitRepository -> [Path Rel File] -> m ()
    commit :: GitRepository -> Text -> m (Maybe Committish)
    findCommitByRef :: GitRepository -> BranchRef -> m (Maybe Committish)
    findCommitByMessage :: GitRepository -> Text -> m (Maybe Committish)
    getCommitMessage :: GitRepository -> Committish -> m Text
    gitDiffHasCommits :: GitRepository -> Committish -> Committish -> m Bool
    gitLogDiff :: GitRepository -> Committish -> Committish -> m Text
    listFiles :: GitRepository -> m [Path Rel File]
    populateRerereFromMerge :: GitRepository -> Committish -> m ()
    getCurrentBranch :: GitRepository -> m Text
    getCurrentHead :: GitRepository -> m Committish
    renameCurrentBranch :: GitRepository -> Text -> m ()
    pushRefs :: GitRepository -> [PushSpec] -> m ()

class Monad m => MonadGitServer m where
    createOrUpdatePullRequest :: GitRepository -> PullRequestSettings -> Committish -> Text -> Text -> Text -> m ()

instance Pretty Committish where
    pretty (Committish c) = pretty c
