{-# LANGUAGE OverloadedStrings #-}

module Data where

import           Data.Text                      ( Text )
import           Data.Set                       ( Set )
import           Data.Map                       ( Map )
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y

data PreliminaryProjectConfiguration    = PreliminaryProjectConfiguration
        { preSelectedTemplate :: Maybe Text
        , preSelectedBaseBranch :: Maybe Text
        , preSelectedFeatureBranches :: Maybe (Set Text)
        , preVariableValues :: Maybe (Map Text Text)
        }
        deriving (Show)

newtype FinalTemplateConfiguration = FinalTemplateConfiguration
        { selectedTemplate :: Text
        }
        deriving (Show)

data FinalProjectConfiguration = FinalProjectConfiguration
        { baseBranch :: Text
        , featureBranches :: Set Text
        , variableValues :: Map Text Text
        }
        deriving (Show)

data TemplateBranchInformation = TemplateBranchInformation
        { branchName :: Text
        , isBaseBranch :: Bool
        , requiredBranches :: Set Text
        , branchVariables :: Map Text Text
        }
        deriving (Show)

newtype TemplateInformation = TemplateInformation
        { branchesInformation :: [TemplateBranchInformation]
        }
        deriving (Show)

data TemplateYaml = TemplateYaml
        { variables :: Map Text Text
        , features :: Set Text
        }
        deriving (Show)

instance FromJSON TemplateYaml where
        parseJSON (Y.Object v) =
                TemplateYaml <$> v .: "variables" <*> v .: "features"
        parseJSON _ = fail "Invalid template YAML definition"
