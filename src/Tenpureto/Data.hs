module Tenpureto.Data where

import           Data.Text                      ( Text )
import           Data.Set                       ( Set )
import           Data.Map                       ( Map )

import           Control.Applicative

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.Logging
import           Tenpureto.TemplateLoader

import           Tenpureto.Orphanage            ( )

data PreliminaryProjectConfiguration = PreliminaryProjectConfiguration
        { preSelectedTemplate :: Maybe Text
        , preTargetDirectory :: Maybe (Path Abs Dir)
        , prePreviousTemplateCommit :: Maybe Committish
        , preSelectedBranches :: Maybe (Set Text)
        , preVariableValues :: Maybe (Map Text Text)
        , preVariableDefaultReplacements :: Map Text Text
        }
        deriving (Show)

data FinalTemplateConfiguration = FinalTemplateConfiguration
        { selectedTemplate :: Text
        , targetDirectory :: Path Abs Dir
        }
        deriving (Show)

newtype FinalUpdateConfiguration = FinalUpdateConfiguration
        { previousTemplateCommit :: Committish
        }
        deriving (Show)

data FinalProjectConfiguration = FinalProjectConfiguration
        { projectBranches :: [TemplateBranchInformation]
        , variableValues :: Map Text Text
        }
        deriving (Show)


instance Semigroup PreliminaryProjectConfiguration where
    (<>) a b = PreliminaryProjectConfiguration
        { preSelectedTemplate = preSelectedTemplate a <|> preSelectedTemplate b
        , preTargetDirectory = preTargetDirectory a <|> preTargetDirectory b
        , prePreviousTemplateCommit = prePreviousTemplateCommit a
                                          <|> prePreviousTemplateCommit b
        , preSelectedBranches = preSelectedBranches a <|> preSelectedBranches b
        , preVariableValues = preVariableValues a <|> preVariableValues b
        , preVariableDefaultReplacements =
            preVariableDefaultReplacements a <> preVariableDefaultReplacements b
        }

instance Pretty PreliminaryProjectConfiguration where
    pretty cfg = (align . vsep)
        [ "Template:                      "
            <+> (align . pretty) (preSelectedTemplate cfg)
        , "Target directory:              "
            <+> (align . pretty) (preTargetDirectory cfg)
        , "Previous template commit:      "
            <+> (align . pretty . show) (prePreviousTemplateCommit cfg)
        , "Selected branches:             "
            <+> (align . pretty) (preSelectedBranches cfg)
        , "Variable values:               "
            <+> (align . pretty) (preVariableValues cfg)
        , "Variable default replacements: "
            <+> (align . pretty) (preVariableDefaultReplacements cfg)
        ]

instance Pretty FinalProjectConfiguration where
    pretty cfg = (align . vsep)
        [ "Branches:       " <+> (align . pretty) (projectBranches cfg)
        , "Variable values:" <+> (align . pretty) (variableValues cfg)
        ]

instance Pretty FinalUpdateConfiguration where
    pretty cfg = (align . vsep)
        [ "Previous template commit:"
              <+> (align . pretty) (previousTemplateCommit cfg)
        ]
