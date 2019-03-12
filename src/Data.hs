{-# LANGUAGE OverloadedStrings #-}

module Data where

import           Data.Set                       ( Set )
import           Data.Map                       ( Map )
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y

data PreliminaryProjectConfiguration    = PreliminaryProjectConfiguration
        { preSelectedTemplate :: Maybe String
        , preSelectedBranches :: Maybe (Set String)
        , preVariableValues :: Maybe (Map String String)
        }
        deriving (Show)

newtype FinalTemplateConfiguration = FinalTemplateConfiguration
        { selectedTemplate :: String
        }
        deriving (Show)

data FinalProjectConfiguration = FinalProjectConfiguration
        { selectedBranches :: Set String
        , variableValues :: Map String String
        }
        deriving (Show)

data TemplateBranchInformation = TemplateBranchInformation
        { branchName :: String
        , requiredBranches :: Set String
        , branchVariables :: Map String String
        }
        deriving (Show)

newtype TemplateInformation = TemplateInformation
        { branchesInformation :: [TemplateBranchInformation]
        }
        deriving (Show)

data TemplateYaml = TemplateYaml
        { variables :: Map String String
        , features :: Set String
        }
        deriving (Show)

instance FromJSON TemplateYaml where
        parseJSON (Y.Object v) =
                TemplateYaml <$> v .: "variables" <*> v .: "features"
        parseJSON _ = fail "Invalid template YAML definition"
