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
        , preSelectedBranches :: Maybe [String]
        , preVariableValues :: Maybe [(String, String)]
        }
        deriving (Show)

newtype FinalTemplateConfiguration = FinalTemplateConfiguration
        { selectedTemplate :: String
        }
        deriving (Show)

data FinalProjectConfiguration = FinalProjectConfiguration
        { selectedBranches :: [String]
        , variableValues :: [(String, String)]
        }
        deriving (Show)

data TemplateBranchInformation = TemplateBranchInformation
        { branchName :: String
        , requiredBranches :: [String]
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
