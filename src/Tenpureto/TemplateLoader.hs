{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.TemplateLoader
    ( module Tenpureto.TemplateLoader
    , FeatureStability(..)
    , TemplateInformation(..)
    , TemplateBranchInformation(..)
    , TemplateYaml(..)
    , TemplateYamlFeature(..)
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
import           Control.Monad.Trans.Maybe
import qualified Data.Yaml                     as Y

import           Path

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
                  | BranchFilterIsBaseBranch
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
    let fb             = features info
    let currentFeature = find ((==) branch . featureName) fb
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
isHiddenBranch = maybe False featureHidden . templateYamlFeature

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

branchesConflict
    :: TemplateBranchInformation -> TemplateBranchInformation -> Bool
branchesConflict a b =
    Set.member (branchName b) (conflicts $ templateYaml a)
        || Set.member (branchName a) (conflicts $ templateYaml b)

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
applyBranchFilter BranchFilterIsBaseBranch    ti = isBaseBranch ti
applyBranchFilter BranchFilterIsFeatureBranch _  = isFeatureBranch
applyBranchFilter BranchFilterIsHiddenBranch  _  = isHiddenBranch
applyBranchFilter BranchFilterIsMergeBranch   ti = isMergeBranch ti
