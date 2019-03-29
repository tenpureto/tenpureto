module Git where

import           Data.ByteString
import           Data.Text                      ( Text )
import           Path

newtype RepositoryUrl = RepositoryUrl Text
newtype GitRepository = GitRepository { repositoryPath :: Path Abs Dir }

class Monad m => MonadGit m where
    withClonedRepository :: RepositoryUrl -> (GitRepository -> m a) -> m a
    withRepository :: Path Abs Dir -> (GitRepository -> m a) -> m a
    initRepository :: Path Abs Dir -> m GitRepository
    listBranches :: GitRepository -> Text -> m [Text]
    checkoutBranch :: GitRepository -> Text -> Text -> m ()
    mergeBranch :: GitRepository -> Text -> ([Path Rel File] -> m ()) -> m ()
    runMergeTool :: GitRepository -> m ()
    getBranchFile :: GitRepository -> Text -> Path Rel File -> m (Maybe ByteString)
    getWorkingCopyFile :: GitRepository -> Path Rel File -> m (Maybe ByteString)
    writeAddFile :: GitRepository -> Path Rel File -> ByteString -> m ()
    addFiles :: GitRepository -> [Path Rel File] -> m ()
    commit :: GitRepository -> Text -> m ()
    findCommit :: GitRepository -> Text -> m (Maybe Text)
