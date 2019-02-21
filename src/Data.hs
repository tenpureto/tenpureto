module Data where

data PreliminaryProjectConfiguration    = PreliminaryProjectConfiguration
        { preSelectedTemplate :: Maybe String
        , preSelectedBranches :: Maybe [String]
        , preVariableValues :: Maybe [(String, String)]
        }

newtype FinalTemplateConfiguration = FinalTemplateConfiguration
        { selectedTemplate :: String
        }

data FinalProjectConfiguration = FinalProjectConfiguration
        { selectedBranches :: [String]
        , variableValues :: [(String, String)]
        }

data TemplateBranchInformation = TemplateBranchInformation
    { branchName :: String
    , requiredBranches :: [String]
    , branchVariables :: [String]
    }

newtype TemplateInformation = TemplateInformation { branchesInformation :: [TemplateBranchInformation] }
