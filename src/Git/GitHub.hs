{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.GitHub where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Maybe
import           Data.List
import qualified Data.ByteString               as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                , (.!=)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Functor
import           Control.Monad.IO.Class
import           Control.Monad.Catch

import           System.Environment
import           System.Exit
import           Path

import           Git                            ( RepositoryUrl(..)
                                                , GitRepository(..)
                                                , Committish(..)
                                                , PushSpec
                                                , BranchRef(..)
                                                , PullRequestSettings(..)
                                                )
import           Logging
import           Tenpureto.Exec

newtype GitHubException = GitHubApiResponseParseException { stdOut :: Text }
    deriving Show

instance Exception GitHubException

data ApiMethod = ApiPost | ApiPatch

apiMethod :: ApiMethod -> Text
apiMethod ApiPost  = "POST"
apiMethod ApiPatch = "PATCH"

newtype PullRequestAssignee = PullRequestAssignee { assigneeLogin :: Text }
newtype PullRequestLabel = PullRequestLabel { labelName :: Text }
data PullRequest = PullRequest { pullRequestNumber :: Int, pullRequestAssignees :: [PullRequestAssignee], pullRequestLabels :: [PullRequestLabel] }

instance FromJSON PullRequestAssignee where
    parseJSON (Aeson.Object v) = PullRequestAssignee <$> v .: "login"

instance FromJSON PullRequestLabel where
    parseJSON (Aeson.Object v) = PullRequestLabel <$> v .: "name"

instance FromJSON PullRequest where
    parseJSON (Aeson.Object v) =
        PullRequest
            <$> v
            .:  "number"
            <*> v
            .:? "assignees"
            .!= []
            <*> v
            .:? "labels"
            .!= []
    parseJSON _ = fail "Invalid template YAML definition"

data ReferenceInputPayload = ReferenceInputPayload { referenceSha :: Text, referenceRef :: Maybe Text }

instance ToJSON ReferenceInputPayload where
    toJSON ReferenceInputPayload { referenceSha = sha, referenceRef = ref } =
        Aeson.object $ ["sha" .= sha] ++ catMaybes [fmap ("ref" .=) ref]

data PullRequestInputPayload = PullRequestInputPayload { pullRequestHead :: Text, pullRequestBase :: Text, pullRequestTitle :: Maybe Text }

instance ToJSON PullRequestInputPayload where
    toJSON PullRequestInputPayload { pullRequestHead = h, pullRequestBase = b, pullRequestTitle = t }
        = Aeson.object $ ["head" .= h, "base" .= b] ++ catMaybes
            [fmap ("title" .=) t]

data IssueInputPayload = IssueInputPayload { issueAssignees :: [Text], issueLabels :: [Text] }

instance ToJSON IssueInputPayload where
    toJSON IssueInputPayload { issueAssignees = a, issueLabels = l } =
        Aeson.object ["assignees" .= a, "labels" .= l]

hubApiCmd
    :: (MonadIO m, MonadThrow m, MonadLog m, ToJSON a)
    => GitRepository
    -> ApiMethod
    -> Text
    -> a
    -> m (ExitCode, ByteString, ByteString)
hubApiCmd (GitRepository path) method endpoint input = runInputCmd
    (  "hub"
    :| [ "-C"
       , T.pack (toFilePath path)
       , "api"
       , "--method"
       , apiMethod method
       , endpoint
       , "--input"
       , "-"
       ]
    )
    (Aeson.encode input)

hubApiGetCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Text
    -> [(Text, Text)]
    -> m (ExitCode, ByteString, ByteString)
hubApiGetCmd (GitRepository path) endpoint fields = runCmd
    (  "hub"
    :| ["-C", T.pack (toFilePath path), "api", "--method", "GET", endpoint]
    ++ (fields >>= \(key, value) -> ["--raw-field", key <> "=" <> value])
    )

createOrUpdateReference
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> Committish
    -> Text
    -> m ()
createOrUpdateReference repo (Committish c) reference = do
    (exitCode, stdout, stderr) <- hubApiCmd
        repo
        ApiPost
        "/repos/{owner}/{repo}/git/refs"
        ReferenceInputPayload { referenceSha = c
                              , referenceRef = Just ("refs/heads/" <> reference)
                              }
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure 22 ->
            hubApiCmd
                    repo
                    ApiPatch
                    ("/repos/{owner}/{repo}/git/refs/heads/" <> reference)
                    ReferenceInputPayload { referenceSha = c
                                          , referenceRef = Nothing
                                          }
                >>= unitOrThrow
        ExitFailure other -> throwM $ ExecException other (decodeUtf8 stderr)

createOrUpdatePullRequest
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => GitRepository
    -> PullRequestSettings
    -> Committish
    -> Text
    -> Text
    -> Text
    -> m ()
createOrUpdatePullRequest repo settings (Committish c) title source target = do
    createOrUpdateReference repo (Committish c) source
    let throwApiError bs =
            throwM $ GitHubApiResponseParseException (decodeUtf8 bs)
        parseApiResponse bs = maybe (throwApiError bs) return (Aeson.decode bs)
    exitingPullRequests <-
        hubApiGetCmd repo
                     "/repos/{owner}/{repo}/pulls"
                     [("head", source), ("base", target)]
        >>= stdoutOrThrow
        >>= parseApiResponse
    pullRequest <- case exitingPullRequests of
        [pullRequest] -> return pullRequest
        [] ->
            hubApiCmd
                    repo
                    ApiPost
                    "/repos/{owner}/{repo}/pulls"
                    PullRequestInputPayload { pullRequestHead  = source
                                            , pullRequestBase  = target
                                            , pullRequestTitle = Just title
                                            }
                >>= stdoutOrThrow
                >>= parseApiResponse
    hubApiCmd
            repo
            ApiPatch
            (  "/repos/{owner}/{repo}/issues/"
            <> (T.pack . show . pullRequestNumber) pullRequest
            )
            IssueInputPayload
                { issueAssignees = nub
                                   $  fmap assigneeLogin
                                           (pullRequestAssignees pullRequest)
                                   ++ pullRequestAssignTo settings
                , issueLabels    = nub
                                   $  fmap labelName
                                           (pullRequestLabels pullRequest)
                                   ++ pullRequestAddLabels settings
                }
        >>= unitOrThrow
    return ()
