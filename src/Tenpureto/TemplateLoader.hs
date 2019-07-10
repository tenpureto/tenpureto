{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.TemplateLoader
    ( module Tenpureto.TemplateLoader
    , FeatureStability(..)
    , TemplateInformation
    , branchesInformation
    , branchesGraph
    , TemplateBranchInformation(..)
    , TemplateYaml(..)
    , TemplateYamlFeature
    , yamlFeatureName
    , requiredBranches
    , branchVariables
    , Graph
    , isFeatureBranch
    , isHiddenBranch
    , isMergeOf
    )
where

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
import           Data.Bifunctor
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Yaml                     as Y

import           Path

import           Tenpureto.Graph
import           Tenpureto.Effects.Git
import           Tenpureto.TemplateLoader.Internal

import           Tenpureto.Orphanage            ( )

data BranchFilter = BranchFilterAny
                  | BranchFilterNone
                  | BranchFilterEqualTo Text
                  | BranchFilterChildOf Text
                  | BranchFilterParentOf Text
                  | BranchFilterOr [BranchFilter]
                  | BranchFilterAnd [BranchFilter]
                  | BranchFilterIsFeatureBranch
                  | BranchFilterIsHiddenBranch
                  | BranchFilterIsMergeBranch

internalBranchPrefix :: Text
internalBranchPrefix = "tenpureto/"

loadTemplateInformation
    :: Members '[Git] r => GitRepository -> Sem r TemplateInformation
loadTemplateInformation repo = do
    allBranches <- listBranches repo
    let branches = filter (not . T.isPrefixOf internalBranchPrefix) allBranches
    branchConfigurations <- traverse (loadBranchConfiguration repo)
        $ sort branches
    let bi = catMaybes branchConfigurations
    return $ templateInformation bi

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
    return $ TemplateBranchInformation { branchName   = branch
                                       , branchCommit = branchHead
                                       , templateYaml = info
                                       }

featureDescription :: TemplateBranchInformation -> Maybe Text
featureDescription = yamlFeatureDescription <=< templateYamlFeature

featureStability :: TemplateBranchInformation -> FeatureStability
featureStability = maybe Stable yamlFeatureStability . templateYamlFeature

isMergeBranch :: TemplateInformation -> TemplateBranchInformation -> Bool
isMergeBranch t = isMergeBranch' (branchesInformation t)

branchesConflict
    :: TemplateBranchInformation -> TemplateBranchInformation -> Bool
branchesConflict a b =
    Set.member (branchName b) (yamlConflicts $ templateYaml a)
        || Set.member (branchName a) (yamlConflicts $ templateYaml b)

managedBranches :: TemplateInformation -> [TemplateBranchInformation]
managedBranches t = filter (\b -> isFeatureBranch b || isMergeBranch t b)
                           (branchesInformation t)

parseTemplateYaml :: ByteString -> Either Text TemplateYaml
parseTemplateYaml yaml =
    let info :: Either Y.ParseException TemplateYaml
        info = Y.decodeEither' (BS.toStrict yaml)
    in  first (T.pack . Y.prettyPrintParseException) info

formatTemplateYaml :: TemplateYaml -> ByteString
formatTemplateYaml y = (BS.fromStrict . Y.encode) TemplateYaml
    { yamlVariables = yamlVariables y
    , yamlFeatures  = Set.filter (not . yamlFeatureHidden) (yamlFeatures y)
    , yamlExcludes  = mempty
    , yamlConflicts = mempty
    }

templateYamlFile :: Path Rel File
templateYamlFile = [relfile|.template.yaml|]

findTemplateBranch
    :: TemplateInformation -> Text -> Maybe TemplateBranchInformation
findTemplateBranch template branch =
    find ((==) branch . branchName) (branchesInformation template)

getBranchParents :: TemplateInformation -> TemplateBranchInformation -> Set Text
getBranchParents template branch =
    let isAncestor b a
            | isFeatureBranch a
            = Set.member (branchName a) (requiredBranches b)
                && (requiredBranches a /= requiredBranches b)
            | otherwise
            = Set.isProperSubsetOf (requiredBranches a) (requiredBranches b)
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
    :: BranchFilter -> TemplateInformation -> [TemplateBranchInformation]
getTemplateBranches f ti =
    filter (applyBranchFilter f ti) (branchesInformation ti)

filterTemplateBranches
    :: BranchFilter -> TemplateInformation -> Graph TemplateBranchInformation
filterTemplateBranches f ti =
    filterVertices (applyBranchFilter f ti) (templateBranchesGraph ti)

applyBranchFilter
    :: BranchFilter -> TemplateInformation -> TemplateBranchInformation -> Bool
applyBranchFilter BranchFilterAny            _ = const True
applyBranchFilter BranchFilterNone           _ = const False
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
applyBranchFilter (BranchFilterOr filters) ti =
    \bi -> any (\f -> applyBranchFilter f ti bi) filters
applyBranchFilter (BranchFilterAnd filters) ti =
    \bi -> all (\f -> applyBranchFilter f ti bi) filters
applyBranchFilter BranchFilterIsFeatureBranch _ =
    \bi -> isFeatureBranch bi && not (isHiddenBranch bi)
applyBranchFilter BranchFilterIsHiddenBranch _  = isHiddenBranch
applyBranchFilter BranchFilterIsMergeBranch  ti = isMergeBranch ti


renameBranchInYaml :: Text -> Text -> TemplateYaml -> TemplateYaml
renameBranchInYaml oldName newName descriptor = TemplateYaml
    { yamlVariables = yamlVariables descriptor
    , yamlFeatures  = Set.map (renameBranch oldName newName)
                              (yamlFeatures descriptor)
    , yamlExcludes  = yamlExcludes descriptor
    , yamlConflicts = yamlConflicts descriptor
    }
  where
    renameBranch old new b = if yamlFeatureName b == old
        then TemplateYamlFeature
            { yamlFeatureName        = new
            , yamlFeatureDescription = yamlFeatureDescription b
            , yamlFeatureHidden      = yamlFeatureHidden b
            , yamlFeatureStability   = yamlFeatureStability b
            }
        else b

replaceInFunctor :: (Functor f, Eq a) => a -> a -> f a -> f a
replaceInFunctor from to = fmap (\v -> if from == v then to else v)

replaceVariableInYaml :: Text -> Text -> TemplateYaml -> TemplateYaml
replaceVariableInYaml old new descriptor = TemplateYaml
    { yamlVariables = replaceInFunctor old new (yamlVariables descriptor)
    , yamlFeatures  = yamlFeatures descriptor
    , yamlExcludes  = yamlExcludes descriptor
    , yamlConflicts = yamlConflicts descriptor
    }

templateBranchesGraph :: TemplateInformation -> Graph TemplateBranchInformation
templateBranchesGraph = branchesGraph
