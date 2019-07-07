module Tenpureto.TemplateLoader.Internal where

import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text.Prettyprint.Doc
import           Data.Yaml                      ( FromJSON(..)
                                                , ToJSON(..)
                                                , (.:?)
                                                , (.!=)
                                                , (.=)
                                                )
import qualified Data.Yaml                     as Y
import           Data.Foldable

import           Tenpureto.Graph
import           Tenpureto.Effects.Git

data FeatureStability = Deprecated | Experimental | Stable
        deriving (Show, Eq, Ord, Enum, Bounded)

data TemplateYamlFeature = TemplateYamlFeature
        { yamlFeatureName :: Text
        , yamlFeatureDescription :: Maybe Text
        , yamlFeatureHidden :: Bool
        , yamlFeatureStability :: FeatureStability
        }
        deriving (Show, Eq, Ord)

data TemplateYaml = TemplateYaml
        { yamlVariables :: Map Text Text
        , yamlFeatures :: Set TemplateYamlFeature
        , yamlExcludes :: Set Text
        , yamlConflicts :: Set Text
        }
        deriving (Show, Eq, Ord)

data TemplateInformation = TemplateInformation
    { branchesInformation :: [TemplateBranchInformation]
    , branchesGraph :: Graph TemplateBranchInformation
    }
    deriving Show

data TemplateBranchInformation = TemplateBranchInformation
    { branchName :: Text
    , branchCommit :: Committish
    , templateYaml :: TemplateYaml
    }
    deriving (Show, Eq, Ord)

instance Pretty TemplateBranchInformation where
    pretty cfg = (align . vsep)
        [ "Branch name:      " <+> (align . pretty) (branchName cfg)
        , "Required branches:" <+> (align . pretty) (requiredBranches cfg)
        , "Branch variables: " <+> (align . pretty) (branchVariables cfg)
        , "Description:      " <+> (align . pretty)
            (yamlFeatureDescription =<< templateYamlFeature cfg)
        , "Hidden:           "
            <+> (align . pretty) (yamlFeatureHidden <$> templateYamlFeature cfg)
        ]

instance Pretty TemplateInformation where
    pretty cfg = (align . vsep)
        ["Branches:" <+> (align . pretty) (branchesInformation cfg)]

instance Pretty TemplateYamlFeature where
    pretty feature =
        (align . vsep) ["Name:" <+> (align . pretty) (yamlFeatureName feature)]

instance Pretty TemplateYaml where
    pretty cfg = (align . vsep)
        [ "Variables:" <+> (align . pretty) (yamlVariables cfg)
        , "Features: " <+> (align . pretty) (yamlFeatures cfg)
        ]

instance FromJSON FeatureStability where
    parseJSON (Y.String "stable"      ) = pure Stable
    parseJSON (Y.String "experimental") = pure Experimental
    parseJSON (Y.String "deprecated"  ) = pure Deprecated
    parseJSON _                         = fail "Invalid feature stability value"

instance FromJSON TemplateYamlFeature where
    parseJSON (Y.String v) = pure $ TemplateYamlFeature
        { yamlFeatureName        = v
        , yamlFeatureDescription = Nothing
        , yamlFeatureHidden      = False
        , yamlFeatureStability   = Stable
        }
    parseJSON (Y.Object v) = case HashMap.toList v of
        [(k, Y.Object vv)] ->
            TemplateYamlFeature k
                <$> vv
                .:? "description"
                <*> vv
                .:? "hidden"
                .!= False
                <*> vv
                .:? "stability"
                .!= Stable
        _ -> fail "Invalid template YAML feature definition"
    parseJSON _ = fail "Invalid template YAML feature definition"

instance FromJSON TemplateYaml where
    parseJSON (Y.Object v) =
        TemplateYaml
            <$> v
            .:? "variables"
            .!= Map.empty
            <*> v
            .:? "features"
            .!= Set.empty
            <*> v
            .:? "excludes"
            .!= Set.empty
            <*> v
            .:? "conflicts"
            .!= Set.empty
    parseJSON _ = fail "Invalid template YAML definition"

instance ToJSON TemplateYamlFeature where
    toJSON TemplateYamlFeature { yamlFeatureName = n } = toJSON n

instance ToJSON TemplateYaml where
    toJSON TemplateYaml { yamlVariables = v, yamlFeatures = f, yamlExcludes = e }
        = Y.object $ catMaybes
            ["variables" .?= v, "features" .?= f, "excludes" .?= e]
        where a .?= b = if b == mempty then Nothing else Just (a .= b)

instance Semigroup TemplateYaml where
    (<>) a b = TemplateYaml
        { yamlVariables = yamlVariables a <> yamlVariables b
        , yamlFeatures  = yamlFeatures a <> yamlFeatures b
        , yamlExcludes  = yamlExcludes a <> yamlExcludes b
        , yamlConflicts = yamlConflicts a <> yamlConflicts b
        }

instance Monoid TemplateYaml where
    mempty = TemplateYaml { yamlVariables = mempty
                          , yamlFeatures  = mempty
                          , yamlExcludes  = mempty
                          , yamlConflicts = mempty
                          }

requiredBranches :: TemplateBranchInformation -> Set Text
requiredBranches = Set.map yamlFeatureName . yamlFeatures . templateYaml

branchVariables :: TemplateBranchInformation -> Map Text Text
branchVariables = yamlVariables . templateYaml

templateYamlFeature :: TemplateBranchInformation -> Maybe TemplateYamlFeature
templateYamlFeature bi =
    find ((==) (branchName bi) . yamlFeatureName) $ yamlFeatures (templateYaml bi)

buildGraph :: [TemplateBranchInformation] -> Graph TemplateBranchInformation
buildGraph bis =
    let
        branchNames = fmap branchName bis
        branchEdges =
            [ (branchName a, branchName b)
            | a <- bis
            , b <- bis
            , Set.member (branchName a) (requiredBranches b)
                || (  Set.isProperSubsetOf (requiredBranches a)
                                           (requiredBranches b)
                   && not (Set.member (branchName b) (requiredBranches b))
                   )
            ]
        branchNameGraph = overlay (vertices branchNames) (edges branchEdges)
        findBranchInformation name = find ((==) name . branchName) bis
    in
        filterMapVertices findBranchInformation branchNameGraph

templateInformation :: [TemplateBranchInformation] -> TemplateInformation
templateInformation branches =
    TemplateInformation branches (buildGraph branches)
