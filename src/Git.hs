module Git where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text )
import           Path
import           Logging

newtype RepositoryUrl = RepositoryUrl Text deriving (Eq, Show)
newtype GitRepository = GitRepository { repositoryPath :: Path Abs Dir }
newtype Committish = Committish Text deriving (Eq, Show)
data RefType = BranchRef deriving (Show)
data Ref = Ref { refType :: RefType, reference :: Text } deriving (Show)
data Refspec = Refspec { sourceRef :: Maybe Committish, destinationRef :: Ref } deriving (Show)

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
    findCommitByRef :: GitRepository -> Ref -> m (Maybe Committish)
    findCommitByMessage :: GitRepository -> Text -> m (Maybe Committish)
    getCommitMessage :: GitRepository -> Committish -> m Text
    getCommitContent :: GitRepository -> Committish -> m Text
    listFiles :: GitRepository -> m [Path Rel File]
    populateRerereFromMerge :: GitRepository -> Committish -> m ()
    getCurrentBranch :: GitRepository -> m Text
    getCurrentHead :: GitRepository -> m Committish
    renameCurrentBranch :: GitRepository -> Text -> m ()
    pushRefs :: GitRepository -> [Refspec] -> m ()

instance Pretty Committish where
    pretty (Committish c) = pretty c

branchRef :: Text -> Ref
branchRef = Ref BranchRef
