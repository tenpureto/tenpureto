{-# LANGUAGE OverloadedStrings #-}

module Tenpureto.Messages where

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Path

import           Data
import           Git

-- Commit messages

commitCreateMessage :: FinalTemplateConfiguration -> Text
commitCreateMessage cfg =
    "Create from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMessage :: FinalTemplateConfiguration -> Text
commitUpdateMessage cfg =
    "Update from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMergeMessage :: FinalTemplateConfiguration -> Text
commitUpdateMergeMessage cfg = "Merge " <> selectedTemplate cfg

commitRenameBranchMessage :: Text -> Text -> Text
commitRenameBranchMessage from to =
    "Rename \"" <> from <> "\" branch to \"" <> to <> "\""

commitChangeVariableMessage :: Text -> Text -> Text
commitChangeVariableMessage from to =
    "Change \"" <> from <> "\" variable to \"" <> to <> "\""

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

noRelevantTemplateChanges :: Doc a
noRelevantTemplateChanges = "There are no relevant changes in the template."

mergeSuccess :: Doc a
mergeSuccess = "Successfully merged."

confirmPushMessage :: [BranchRef] -> [BranchRef] -> [BranchRef] -> Doc a
confirmPushMessage deletes creates updates = "Do you want to"
    <+> (fillSep . punctuate " and") (toDelete ++ toCreate ++ toUpdate)
  where
    branchList =
        fillSep . punctuate comma . fmap (dquotes . pretty . reference)
    toDelete = if null deletes then [] else ["delete" <+> branchList deletes]
    toCreate = if null deletes then [] else ["create" <+> branchList creates]
    toUpdate = if null updates then [] else ["push to" <+> branchList updates]

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
