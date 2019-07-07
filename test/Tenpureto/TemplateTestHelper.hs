module Tenpureto.TemplateTestHelper where

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set

import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                , FeatureStability(..)
                                                , requiredBranches
                                                )
import           Tenpureto.TemplateLoader.Internal
                                                ( TemplateYaml(..), TemplateYamlFeature(..) )

yamlFeature :: Text -> TemplateYamlFeature
yamlFeature name = TemplateYamlFeature { yamlFeatureName        = name
                                       , yamlFeatureDescription = Nothing
                                       , yamlFeatureHidden      = False
                                       , yamlFeatureStability   = Stable
                                       }

anonymousBranch :: Text -> [Text] -> TemplateBranchInformation
anonymousBranch name deps = TemplateBranchInformation
    { branchName          = name
    , branchCommit        = Committish "undefined"
    , templateYaml        = TemplateYaml    { yamlVariables = mempty
                                            , yamlFeatures = Set.fromList $ fmap yamlFeature deps
                                            , yamlExcludes = mempty
                                            , yamlConflicts = mempty
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
