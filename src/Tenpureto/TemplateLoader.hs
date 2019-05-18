{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.TemplateLoader where

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
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , ToJSON(..)
                                                , (.:?)
                                                , (.!=)
                                                , (.=)
                                                )
import           Data.Foldable
import           Data.Bifunctor

import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe

import           Path

import           Git
import           Logging

data TenpuretoTemplateException = InvalidTemplateException Text Text
                                    deriving (Exception)

instance Show TenpuretoTemplateException where
    show (InvalidTemplateException template reason) =
        T.unpack $ "Template \"" <> template <> "\" is not valid: " <> reason

data BranchFilter = BranchFilterChildOf Text | BranchFilterParentOf Text

data TemplateYaml = TemplateYaml
        { variables :: InsOrdHashMap Text Text
        , features :: Set Text
        }
        deriving (Show, Eq)

data TemplateBranchInformation = TemplateBranchInformation
    { branchName :: Text
    , isBaseBranch :: Bool
    , isFeatureBranch :: Bool
    , requiredBranches :: Set Text
    , branchVariables :: InsOrdHashMap Text Text
    , templateYaml :: TemplateYaml
    }
    deriving (Show, Eq)

newtype TemplateInformation = TemplateInformation
    { branchesInformation :: [TemplateBranchInformation]
    }
    deriving (Show, Eq)

loadTemplateInformation
    :: (MonadThrow m, MonadGit m)
    => Text
    -> GitRepository
    -> m TemplateInformation
loadTemplateInformation repositoryName repository = do
    branches             <- listBranches repository
    branchConfigurations <- traverse (loadBranchConfiguration repository)
        $ sort branches
    let bi = catMaybes branchConfigurations
    if hasBaseBranches bi
        then return ()
        else throwM $ InvalidTemplateException repositoryName
                                               "no base branches found"
    return $ TemplateInformation { branchesInformation = bi }
  where
    hasBaseBranches :: [TemplateBranchInformation] -> Bool
    hasBaseBranches = any isBaseBranch

loadBranchConfiguration
    :: (MonadThrow m, MonadGit m)
    => GitRepository
    -> Text
    -> m (Maybe TemplateBranchInformation)
loadBranchConfiguration repo branch = runMaybeT $ do
    descriptor <- MaybeT $ getBranchFile
        repo
        (T.pack "remotes/origin/" <> branch)
        templateYamlFile
    info <- MaybeT . return . rightToMaybe $ parseTemplateYaml descriptor
    let fb = features info
    return $ TemplateBranchInformation
        { branchName       = branch
        , isBaseBranch     = fb == Set.singleton branch
        , isFeatureBranch  = Set.member branch fb
        , requiredBranches = fb
        , branchVariables  = variables info
        , templateYaml     = info
        }

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
    let getAncestors :: TemplateBranchInformation -> [TemplateBranchInformation]
        getAncestors b = filter
            (flip Set.isProperSubsetOf (requiredBranches b) . requiredBranches)
            (branchesInformation template)
        ancestors         = getAncestors branch
        indirectAncestors = mconcat $ getAncestors <$> ancestors
    in  Set.fromList (fmap branchName ancestors)
            `Set.difference` Set.fromList (fmap branchName indirectAncestors)

getBranchChildren
    :: TemplateInformation -> TemplateBranchInformation -> Set Text
getBranchChildren template branch = Set.fromList $ branchName <$> filter
    (Set.member (branchName branch) . getBranchParents template)
    (branchesInformation template)

getTemplateBranches
    :: [BranchFilter] -> TemplateInformation -> [TemplateBranchInformation]
getTemplateBranches [] ti = branchesInformation ti
getTemplateBranches (head : tail) ti =
    filter (applyBranchFilter head ti) $ getTemplateBranches tail ti
  where
    applyBranchFilter
        :: BranchFilter
        -> TemplateInformation
        -> TemplateBranchInformation
        -> Bool
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
        , "Base branch:      " <+> (align . pretty) (isBaseBranch cfg)
        , "Required branches:" <+> (align . pretty) (requiredBranches cfg)
        , "Branch variables: " <+> (align . pretty) (branchVariables cfg)
        ]

instance Pretty TemplateInformation where
    pretty cfg = (align . vsep)
        ["Branches:" <+> (align . pretty) (branchesInformation cfg)]

instance Pretty TemplateYaml where
    pretty cfg = (align . vsep)
        [ "Variables:" <+> (align . pretty) (variables cfg)
        , "Features: " <+> (align . pretty) (features cfg)
        ]

instance FromJSON TemplateYaml where
    parseJSON (Y.Object v) =
        TemplateYaml <$> v .:? "variables" .!= InsOrdHashMap.empty <*> v .:? "features" .!= Set.empty
    parseJSON _ = fail "Invalid template YAML definition"

instance ToJSON TemplateYaml where
    toJSON TemplateYaml { variables = v, features = f } =
        Y.object ["variables" .= v, "features" .= f]

instance Semigroup TemplateYaml where
    (<>) a b = TemplateYaml { variables = variables a <> variables b
                            , features  = features a <> features b
                            }

instance Monoid TemplateYaml where
    mempty = TemplateYaml { variables = mempty, features = mempty }
