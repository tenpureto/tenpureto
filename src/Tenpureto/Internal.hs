module Tenpureto.Internal where

import           Polysemy
import           Polysemy.Error

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.TemplateLoader

data TenpuretoException = TemplateBranchNotFoundException Text
                        | TenpuretoBranchNotCreated Text
                        | TenpuretoBranchNotDeleted Text
                        | TenpuretoEmptySelection
                        | TenpuretoEmptyChangeset
                        | TemplateException TenpuretoTemplateException
                        | TenpuretoGitException GitException
                        | TenpuretoUIException UIException
                        | MultipleExceptions [TenpuretoException]
                        | CancelledException

instance Pretty TenpuretoException where
    pretty (TemplateBranchNotFoundException branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not found"
    pretty (TenpuretoBranchNotCreated branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not created"
    pretty (TenpuretoBranchNotDeleted branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not deleted"
    pretty TenpuretoEmptySelection =
        "Cannot create a project from an empty selection"
    pretty TenpuretoEmptyChangeset =
        "Cannot create a commit because the changeset is empty"
    pretty (TemplateException     e) = pretty e
    pretty (TenpuretoGitException e) = pretty e
    pretty (TenpuretoUIException  e) = pretty e
    pretty (MultipleExceptions ee) =
        "Multiple failures:\n" <> (align . vsep) (fmap pretty ee)
    pretty CancelledException = "Cancelled by a user"

loadTemplateInformation'
    :: Members '[Git, Error TenpuretoException] r
    => Text
    -> GitRepository
    -> Sem r TemplateInformation
loadTemplateInformation' repositoryName repo = runErrorAsAnother
    TemplateException
    (loadTemplateInformation repositoryName repo)
