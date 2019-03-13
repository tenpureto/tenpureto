module Git where

import           Data.ByteString
import           Data.Text                      ( Text )

newtype RepositoryUrl = RepositoryUrl Text
newtype GitRepository = GitRepository FilePath

class Monad m => MonadGit m where
    withClonedRepository :: RepositoryUrl -> (GitRepository -> m a) -> m a
    listBranches :: GitRepository -> Text -> m [Text]
    checkoutBranch :: GitRepository -> Text -> Text -> m ()
    mergeBranch :: GitRepository -> Text -> m ()
    getBranchFile :: GitRepository -> Text -> Text -> m (Maybe ByteString)
