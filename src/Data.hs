{-# LANGUAGE OverloadedStrings #-}

module Data where

import           Data.Text                      ( Text )
import           Data.Set                       ( Set )
import           Data.Map                       ( Map )
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )

import           Control.Applicative

import           Path

import           Git                            ( Committish )
import           Logging

data PreliminaryProjectConfiguration = PreliminaryProjectConfiguration
        { preSelectedTemplate :: Maybe Text
        , preTargetDirectory :: Maybe (Path Abs Dir)
        , prePreviousTemplateCommit :: Maybe Committish
        , preSelectedBranches :: Maybe (Set Text)
        , preVariableValues :: Maybe (Map Text Text)
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
        { baseBranch :: Text
        , featureBranches :: Set Text
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
        }

instance Pretty PreliminaryProjectConfiguration where
    pretty cfg = (align . vsep)
        [ "Template:                 "
            <+> (align . pretty) (preSelectedTemplate cfg)
        , "Target directory:         "
            <+> (align . pretty) (preTargetDirectory cfg)
        , "Previous template commit: "
            <+> (align . pretty . show) (prePreviousTemplateCommit cfg)
        , "Selected branches:        "
            <+> (align . pretty) (preSelectedBranches cfg)
        , "Variable values:          "
            <+> (align . pretty) (preVariableValues cfg)
        ]

instance Pretty FinalProjectConfiguration where
    pretty cfg = (align . vsep)
        [ "Base branch:     " <+> (align . pretty) (baseBranch cfg)
        , "Feature branches:" <+> (align . pretty) (featureBranches cfg)
        , "Variable values: " <+> (align . pretty) (variableValues cfg)
        ]

instance Pretty FinalUpdateConfiguration where
    pretty cfg = (align . vsep)
        [ "Previous template commit:"
              <+> (align . pretty) (previousTemplateCommit cfg)
        ]
