module Data where

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
        , branchVariables :: [String]
        }
        deriving (Show)

newtype TemplateInformation = TemplateInformation
        { branchesInformation :: [TemplateBranchInformation]
        }
        deriving (Show)
