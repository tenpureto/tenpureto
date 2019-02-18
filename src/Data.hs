module Data where

data PreliminaryProjectConfiguration    = PreliminaryProjectConfiguration
        { preSelectedTemplate :: Maybe String
        , preSelectedBranches :: [String]
        , preVariableValues :: [(String, String)]
        }
data FinalProjectConfiguration = FinalProjectConfiguration
        { selectedTemplate :: String
        , selectedBranches :: [String]
        , variableValues :: [(String, String)]
        }
