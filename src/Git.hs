module Git where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text )
import           Path
import           Logging

newtype RepositoryUrl = RepositoryUrl Text
newtype GitRepository = GitRepository { repositoryPath :: Path Abs Dir }
newtype Committish = Committish Text deriving (Show)

class Monad m => MonadGit m where
    withClonedRepository :: RepositoryUrl -> (GitRepository -> m a) -> m a
    withNewWorktree :: GitRepository -> Committish -> (GitRepository -> m a) -> m a
    withRepository :: Path Abs Dir -> (GitRepository -> m a) -> m a
    initRepository :: Path Abs Dir -> m GitRepository
    listBranches :: GitRepository -> m [Text]
    checkoutBranch :: GitRepository -> Text -> Text -> m ()
    mergeBranch :: GitRepository -> Text -> ([Path Rel File] -> m ()) -> m ()
    runMergeTool :: GitRepository -> m ()
    getBranchFile :: GitRepository -> Text -> Path Rel File -> m (Maybe ByteString)
    getWorkingCopyFile :: GitRepository -> Path Rel File -> m (Maybe ByteString)
    writeAddFile :: GitRepository -> Path Rel File -> ByteString -> m ()
    addFiles :: GitRepository -> [Path Rel File] -> m ()
    commit :: GitRepository -> Text -> m (Maybe Committish)
    findCommit :: GitRepository -> Text -> m (Maybe Committish)
    listFiles :: GitRepository -> m [Path Rel File]

instance Pretty Committish where
    pretty (Committish c) = pretty c
