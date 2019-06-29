module Tenpureto.TemplateTestHelper where

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set

import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                , TemplateYamlFeature(..)
                                                )

anonymousBranch :: Text -> [Text] -> TemplateBranchInformation
anonymousBranch name deps = TemplateBranchInformation
    { branchName          = name
    , branchCommit        = Committish "undefined"
    , requiredBranches    = Set.fromList deps
    , branchVariables     = mempty
    , templateYaml        = mempty
    , templateYamlFeature = TemplateYamlFeature { featureName   = name
                                                , featureHidden = False
                                                }
    }

branch :: Text -> [Text] -> TemplateBranchInformation
branch name deps = anonymousBranch name (name : deps)

baseBranch :: Text -> TemplateBranchInformation
baseBranch name = branch name []

childBranch :: Text -> [TemplateBranchInformation] -> TemplateBranchInformation
childBranch name parents =
    branch name (Set.toList $ mconcat $ fmap requiredBranches parents)

renamedBranch :: Text -> TemplateBranchInformation -> TemplateBranchInformation
renamedBranch name parent =
    anonymousBranch name (Set.toList $ requiredBranches parent)

mergeBranch :: Text -> [TemplateBranchInformation] -> TemplateBranchInformation
mergeBranch name branches =
    anonymousBranch name (Set.toList $ mconcat $ fmap requiredBranches branches)
