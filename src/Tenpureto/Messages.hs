module Tenpureto.Messages where

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Path

import           Tenpureto.Orphanage            ( )

-- Commit messages

commitCreateMessage :: Text -> Text
commitCreateMessage template =
    "Create from a template\n\nTemplate: " <> template

commitUpdateMessage :: Text -> Text
commitUpdateMessage template =
    "Update from a template\n\nTemplate: " <> template

commitUpdateMergeMessage :: Text -> Text
commitUpdateMergeMessage template = "Merge " <> template

commitRenameBranchMessage :: Text -> Text -> Text
commitRenameBranchMessage from to =
    "Rename \"" <> from <> "\" branch to \"" <> to <> "\""

commitChangeVariableMessage :: Text -> Text -> Text
commitChangeVariableMessage from to =
    "Change \"" <> from <> "\" variable to \"" <> to <> "\""

commitMergeMessage :: Text -> Text -> Text
commitMergeMessage from to = "Merge branch '" <> from <> "' into " <> to

commitUpdateTemplateYaml :: Text
commitUpdateTemplateYaml = "Update .template.yaml"

-- Pull request messages

pullRequestChangeVariableTitle :: Text -> Text -> Text -> Text
pullRequestChangeVariableTitle branch from to =
    commitChangeVariableMessage from to <> " on \"" <> branch <> "\""

pullRequestRenameBranchTitle :: Text -> Text -> Text -> Text
pullRequestRenameBranchTitle branch from to =
    commitRenameBranchMessage from to <> " on \"" <> branch <> "\""

pullRequestBranchIntoBranchTitle :: Text -> Text -> Text
pullRequestBranchIntoBranchTitle from to =
    "Merge \"" <> from <> "\" into \"" <> to <> "\""

pullRequestBranchUpdateTitle :: Text -> Text
pullRequestBranchUpdateTitle to = "Update \"" <> to <> "\""


-- UI messages

changesForBranchMessage :: Text -> Text -> Doc a
changesForBranchMessage branch content =
    "Changes for"
        <+> pretty branch
        <>  ":"
        <>  line
        <>  (indent 4 . pretty) content

projectCreated :: Path t Dir -> Doc a
projectCreated dir = "Created" <+> pretty dir <> "."

projectUpdated :: Path t Dir -> Doc a
projectUpdated dir = "Updated" <+> pretty dir <> "."

projectUpdatedWithConflicts :: Path t Dir -> Doc a
projectUpdatedWithConflicts dir =
    "Updated"
        <+> pretty dir
        <> " but there are merge conflicts, please resolve them manually and commit changes."

noRelevantTemplateChanges :: Doc a
noRelevantTemplateChanges = "There are no relevant changes in the template."

mergeSuccess :: Doc a
mergeSuccess = "Successfully merged."

confirmPushMessage :: Pretty r => [r] -> [r] -> [r] -> Doc a
confirmPushMessage deletes creates updates = "Do you want to"
    <+> (fillSep . punctuate " and") (toDelete ++ toCreate ++ toUpdate)
  where
    toDelete = [ "delete" <+> branchList deletes | not (null deletes) ]
    toCreate = [ "create" <+> branchList creates | not (null creates) ]
    toUpdate = [ "push to" <+> branchList updates | not (null updates) ]

confirmPullRequestMessage :: Pretty r => [r] -> Int -> Doc a
confirmPullRequestMessage updates cleanups = "Do you want to"
    <+> (fillSep . punctuate " and") (toUpdate ++ toCleanup)
  where
    toUpdate =
        [ "update pull requests to" <+> branchList updates
        | not (null updates)
        ]
    toCleanup =
        [ "potentially cleanup" <+> pretty cleanups <+> "pull requests"
        | cleanups > 0
        ]

confirmShellToAmendMessage :: Doc a
confirmShellToAmendMessage = "Do you want to enter a shell to amend the commit"

createBranchManually :: Text -> Doc a
createBranchManually branch =
    "Cannot create a branch with a pull request, please create a \""
        <> pretty branch
        <> "\" branch manually."

deleteBranchManually :: Text -> Doc a
deleteBranchManually branch =
    "Cannot delete a branch with a pull request, please delete a \""
        <> pretty branch
        <> "\" branch manually."

noConflictingCombinations :: Doc a
noConflictingCombinations =
    "All good, there are no feature combinations that have merge conflicts."

conflictingCombinations :: Doc a
conflictingCombinations =
    "The following feature combinations have merge conflicts:"

propagateMergeFailed :: Text -> Text -> Doc a
propagateMergeFailed src dst =
    "Cannot merge" <+> dquotes (pretty src) <+> "into" <+> dquotes (pretty dst)

-- Utilities

branchList :: Pretty r => [r] -> Doc a
branchList = fillSep . punctuate comma . fmap (dquotes . pretty)
