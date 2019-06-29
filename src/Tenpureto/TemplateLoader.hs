{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.TemplateLoader where

import           Polysemy

import           Data.List
import           Data.Maybe
import           Data.Either.Combinators
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , ToJSON(..)
                                                , (.:?)
                                                , (.!=)
                                                , (.=)
                                                )
import           Data.Bifunctor
import           Control.Monad.Trans.Maybe

import           Path

import           Tenpureto.Effects.Logging
import           Tenpureto.Effects.Git

import           Tenpureto.Orphanage            ( )

data BranchFilter = BranchFilterEqualTo Text
                  | BranchFilterChildOf Text
                  | BranchFilterParentOf Text

data TemplateYamlFeature = TemplateYamlFeature
        { featureName :: Text
        , featureHidden :: Bool
        }
        deriving (Show, Eq, Ord)

data TemplateYaml = TemplateYaml
        { variables :: InsOrdHashMap Text Text
        , features :: Set TemplateYamlFeature
        , excludes :: Set Text
        }
        deriving (Show, Eq)

data TemplateBranchInformation = TemplateBranchInformation
    { branchName :: Text
    , branchCommit :: Committish
    , requiredBranches :: Set Text
    , branchVariables :: InsOrdHashMap Text Text
    , templateYaml :: TemplateYaml
    , templateYamlFeature :: TemplateYamlFeature
    }
    deriving (Show, Eq)

newtype TemplateInformation = TemplateInformation
    { branchesInformation :: [TemplateBranchInformation]
    }
    deriving (Show, Eq)

internalBranchPrefix :: Text
internalBranchPrefix = "tenpureto/"

loadTemplateInformation
    :: Members '[Git] r => GitRepository -> Sem r TemplateInformation
loadTemplateInformation repo = do
    allBranches <- listBranches repo
    let branches = filter (not . T.isPrefixOf internalBranchPrefix) allBranches
    branchConfigurations <-
        traverse (loadBranchConfiguration repo) $ sort $ branches
    let bi = catMaybes branchConfigurations
    return $ TemplateInformation { branchesInformation = bi }

loadBranchConfiguration
    :: Members '[Git] r
    => GitRepository
    -> Text
    -> Sem r (Maybe TemplateBranchInformation)
loadBranchConfiguration repo branch = runMaybeT $ do
    branchHead <- MaybeT
        $ findCommitByRef repo (BranchRef $ T.pack "remotes/origin/" <> branch)
    descriptor <- MaybeT $ getRepositoryFile repo branchHead templateYamlFile
    info       <- MaybeT . return . rightToMaybe $ parseTemplateYaml descriptor
    let fb = features info
    currentFeature <- MaybeT . return $ find ((==) branch . featureName) fb
    return $ TemplateBranchInformation
        { branchName          = branch
        , branchCommit        = branchHead
        , requiredBranches    = Set.map featureName fb
        , branchVariables     = variables info
        , templateYaml        = info
        , templateYamlFeature = currentFeature
        }

isBaseBranch :: TemplateInformation -> TemplateBranchInformation -> Bool
isBaseBranch ti b = (Set.filter visible . requiredBranches) b
    == Set.singleton (branchName b)
    where visible = maybe False (not . isHiddenBranch) . findTemplateBranch ti

isFeatureBranch :: TemplateBranchInformation -> Bool
isFeatureBranch b = branchName b `Set.member` requiredBranches b

isHiddenBranch :: TemplateBranchInformation -> Bool
isHiddenBranch = featureHidden . templateYamlFeature

isMergeOf :: TemplateBranchInformation -> [TemplateBranchInformation] -> Bool
isMergeOf bi bis =
    foldMap requiredBranches bis
        == requiredBranches bi
        && all ((/=) (requiredBranches bi) . requiredBranches) bis

isMergeBranch :: TemplateInformation -> TemplateBranchInformation -> Bool
isMergeBranch t b = any (isMergeOf b) mergeOptions
  where
    fb           = filter isFeatureBranch (branchesInformation t)
    mergeOptions = filter ((<) 1 . length) (subsequences fb)

managedBranches :: TemplateInformation -> [TemplateBranchInformation]
managedBranches t = filter (\b -> isFeatureBranch b || isMergeBranch t b)
                           (branchesInformation t)

parseTemplateYaml :: ByteString -> Either Text TemplateYaml
parseTemplateYaml yaml =
    let info :: Either Y.ParseException TemplateYaml
        info = Y.decodeEither' (BS.toStrict yaml)
    in  first (T.pack . Y.prettyPrintParseException) info

formatTemplateYaml :: TemplateYaml -> ByteString
formatTemplateYaml = BS.fromStrict . Y.encode

templateYamlFile :: Path Rel File
templateYamlFile = [relfile|.template.yaml|]

findTemplateBranch
    :: TemplateInformation -> Text -> Maybe TemplateBranchInformation
findTemplateBranch template branch =
    find ((==) branch . branchName) (branchesInformation template)

getBranchParents :: TemplateInformation -> TemplateBranchInformation -> Set Text
getBranchParents template branch =
    let isAncestor b =
                flip Set.isProperSubsetOf (requiredBranches b) . requiredBranches
        getAncestors b = filter (isAncestor b) (managedBranches template)
        ancestors         = getAncestors branch
        indirectAncestors = mconcat $ getAncestors <$> ancestors
    in  Set.fromList (fmap branchName ancestors) `Set.difference` Set.fromList
            (fmap branchName indirectAncestors)

getBranchChildren
    :: TemplateInformation -> TemplateBranchInformation -> Set Text
getBranchChildren template branch = Set.fromList $ branchName <$> filter
    (Set.member (branchName branch) . getBranchParents template)
    (managedBranches template)

getTemplateBranches
    :: [BranchFilter] -> TemplateInformation -> [TemplateBranchInformation]
getTemplateBranches [] ti = branchesInformation ti
getTemplateBranches (h : t) ti =
    filter (applyBranchFilter h ti) $ getTemplateBranches t ti

applyBranchFilter
    :: BranchFilter -> TemplateInformation -> TemplateBranchInformation -> Bool
applyBranchFilter (BranchFilterEqualTo name) _ = (==) name . branchName
applyBranchFilter (BranchFilterChildOf parentBranch) ti =
    let parentNames = maybe Set.empty
                            (getBranchChildren ti)
                            (findTemplateBranch ti parentBranch)
    in  \b -> Set.member (branchName b) parentNames
applyBranchFilter (BranchFilterParentOf childBranch) ti =
    let parentNames = maybe Set.empty
                            (getBranchParents ti)
                            (findTemplateBranch ti childBranch)
    in  \b -> Set.member (branchName b) parentNames

instance Pretty TemplateBranchInformation where
    pretty cfg = (align . vsep)
        [ "Branch name:      " <+> (align . pretty) (branchName cfg)
        , "Required branches:" <+> (align . pretty) (requiredBranches cfg)
        , "Branch variables: " <+> (align . pretty) (branchVariables cfg)
        , "Hidden:           " <+> (align . pretty) (isHiddenBranch cfg)
        ]

instance Pretty TemplateInformation where
    pretty cfg = (align . vsep)
        ["Branches:" <+> (align . pretty) (branchesInformation cfg)]

instance Pretty TemplateYamlFeature where
    pretty feature =
        (align . vsep) ["Name:" <+> (align . pretty) (featureName feature)]

instance Pretty TemplateYaml where
    pretty cfg = (align . vsep)
        [ "Variables:" <+> (align . pretty) (variables cfg)
        , "Features: " <+> (align . pretty) (features cfg)
        ]

instance FromJSON TemplateYamlFeature where
    parseJSON (Y.String v) =
        pure $ TemplateYamlFeature { featureName = v, featureHidden = False }
    parseJSON (Y.Object v) = case HashMap.toList v of
        [(k, Y.Object vv)] ->
            TemplateYamlFeature k <$> vv .:? "hidden" .!= False
        _ -> fail "Invalid template YAML feature definition"
    parseJSON _ = fail "Invalid template YAML feature definition"

instance FromJSON TemplateYaml where
    parseJSON (Y.Object v) =
        TemplateYaml
            <$> v
            .:? "variables"
            .!= InsOrdHashMap.empty
            <*> v
            .:? "features"
            .!= Set.empty
            <*> v
            .:? "excludes"
            .!= Set.empty
    parseJSON _ = fail "Invalid template YAML definition"

instance ToJSON TemplateYamlFeature where
    toJSON TemplateYamlFeature { featureName = n } = toJSON n

instance ToJSON TemplateYaml where
    toJSON TemplateYaml { variables = v, features = f, excludes = e } =
        Y.object $ catMaybes
            ["variables" .?= v, "features" .?= f, "excludes" .?= e]
        where a .?= b = if b == mempty then Nothing else Just (a .= b)

instance Semigroup TemplateYaml where
    (<>) a b = TemplateYaml { variables = variables a <> variables b
                            , features  = features a <> features b
                            , excludes  = excludes a <> excludes b
                            }

instance Monoid TemplateYaml where
    mempty = TemplateYaml { variables = mempty
                          , features  = mempty
                          , excludes  = mempty
                          }
