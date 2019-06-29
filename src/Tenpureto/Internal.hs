module Tenpureto.Internal where

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI

data TenpuretoException = TemplateBranchNotFoundException Text
                        | TenpuretoBranchNotCreated Text
                        | TenpuretoBranchNotDeleted Text
                        | TenpuretoEmptySelection
                        | TenpuretoEmptyChangeset
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
    pretty (TenpuretoGitException e) = pretty e
    pretty (TenpuretoUIException  e) = pretty e
    pretty (MultipleExceptions ee) =
        "Multiple failures:\n" <> (align . vsep) (fmap pretty ee)
    pretty CancelledException = "Cancelled by a user"
