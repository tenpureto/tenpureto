{-# LANGUAGE OverloadedStrings #-}

module Git where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Text                      ( Text )
import           Path
import           Logging

newtype RepositoryUrl = RepositoryUrl Text deriving (Eq, Show)
newtype GitRepository = GitRepository { repositoryPath :: Path Abs Dir }
newtype Committish = Committish Text deriving (Show)
data RefType = BranchRef deriving (Show)
data Ref = Ref RefType Text deriving (Show)
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
    getBranchFile :: GitRepository -> Text -> Path Rel File -> m (Maybe ByteString)
    getWorkingCopyFile :: GitRepository -> Path Rel File -> m (Maybe ByteString)
    writeAddFile :: GitRepository -> Path Rel File -> ByteString -> m ()
    addFiles :: GitRepository -> [Path Rel File] -> m ()
    commit :: GitRepository -> Text -> m (Maybe Committish)
    findCommit :: GitRepository -> Text -> m (Maybe Committish)
    getCommitMessage :: GitRepository -> Committish -> m Text
    getCommitContent :: GitRepository -> Committish -> m Text
    listFiles :: GitRepository -> m [Path Rel File]
    populateRerereFromMerge :: GitRepository -> Committish -> m ()
    getCurrentBranch :: GitRepository -> m Text
    renameCurrentBranch :: GitRepository -> Text -> m ()
    pushRefs :: GitRepository -> [Refspec] -> m ()

instance Pretty Committish where
    pretty (Committish c) = pretty c

branchRef :: Text -> Ref
branchRef = Ref BranchRef
