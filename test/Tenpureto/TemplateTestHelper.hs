module Tenpureto.TemplateTestHelper where

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set

import           Tenpureto.TemplateLoader       ( TemplateInformation(..)
                                                , TemplateBranchInformation(..)
                                                )

branch :: Text -> [Text] -> TemplateBranchInformation
branch name deps = TemplateBranchInformation
    { branchName       = name
    , isBaseBranch     = False
    , isFeatureBranch  = False
    , requiredBranches = Set.insert name $ Set.fromList deps
    , branchVariables  = mempty
    , templateYaml     = mempty
    }

mergeBranch :: Text -> [Text] -> TemplateBranchInformation
mergeBranch name deps = TemplateBranchInformation
    { branchName       = name
    , isBaseBranch     = False
    , isFeatureBranch  = False
    , requiredBranches = Set.fromList deps
    , branchVariables  = mempty
    , templateYaml     = mempty
    }
