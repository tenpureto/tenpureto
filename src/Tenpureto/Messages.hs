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

-- UI messages

commitOnBranchMessage :: Text -> Text -> Doc a
commitOnBranchMessage branch content =
    "Commit on" <+> pretty branch <> ":" <> line <> (indent 4 . pretty) content

projectCreated :: Path t Dir -> Doc a
projectCreated dir = "Created" <+> pretty dir <> "."

projectUpdated :: Path t Dir -> Doc a
projectUpdated dir = "Updated" <+> pretty dir <> "."

noRelevantTemplateChanges :: Doc a
noRelevantTemplateChanges = "There are no relevant changes in the template."

mergeSuccess :: Doc a
mergeSuccess = "Successfully merged."

confirmPushMessage :: [Refspec] -> [Refspec] -> Doc a
confirmPushMessage deletes updates = "Do you want to"
    <+> (fillSep . punctuate " and") (toDelete ++ toUpdate)
  where
    branchList = fillSep . punctuate comma . fmap
        (dquotes . pretty . reference . destinationRef)
    toDelete = if null deletes then [] else ["delete" <+> branchList deletes]
    toUpdate = if null updates then [] else ["push to" <+> branchList updates]

confirmShellToAmendMessage :: Doc a
confirmShellToAmendMessage = "Do you want to enter a shell to amend the commit"
