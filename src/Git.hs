module Git where

newtype RepositoryUrl = RepositoryUrl String
newtype GitRepository = GitRepository FilePath

class Monad m => MonadGit m where
    withClonedRepository :: RepositoryUrl -> (GitRepository -> m a) -> m a
    listBranches :: GitRepository -> String -> m [String]
