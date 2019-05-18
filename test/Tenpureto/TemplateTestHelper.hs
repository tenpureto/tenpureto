{-# LANGUAGE OverloadedStrings #-}
module Tenpureto.TemplateTestHelper where

import           Data.Text                      ( Text )
import qualified Data.Set                      as Set

import           Git                            ( Committish(..) )
import           Tenpureto.TemplateLoader       ( TemplateInformation(..)
                                                , TemplateBranchInformation(..)
                                                )

branch :: Text -> [Text] -> TemplateBranchInformation
branch name deps = TemplateBranchInformation
    { branchName       = name
    , branchCommit     = Committish "undefined"
    , isBaseBranch     = False
    , isFeatureBranch  = False
    , requiredBranches = Set.insert name $ Set.fromList deps
    , branchVariables  = mempty
    , templateYaml     = mempty
    }

mergeBranch :: Text -> [Text] -> TemplateBranchInformation
mergeBranch name deps = TemplateBranchInformation
    { branchName       = name
    , branchCommit     = Committish "undefined"
    , isBaseBranch     = False
    , isFeatureBranch  = False
    , requiredBranches = Set.fromList deps
    , branchVariables  = mempty
    , templateYaml     = mempty
    }
