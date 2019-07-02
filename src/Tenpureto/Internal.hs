{-# LANGUAGE TupleSections #-}

module Tenpureto.Internal where

import           Data.Maybe
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.Dot
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI

data TenpuretoException = TemplateBranchNotFoundException Text
                        | TenpuretoBranchNotCreated Text
                        | TenpuretoBranchNotDeleted Text
                        | TenpuretoEmptySelection
                        | TenpuretoEmptyChangeset
                        | TenpuretoGitException GitException
                        | TenpuretoUIException UIException
                        | MultipleExceptions [TenpuretoException]
                        | CancelledException

instance Pretty TenpuretoException where
    pretty (TemplateBranchNotFoundException branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not found"
    pretty (TenpuretoBranchNotCreated branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not created"
    pretty (TenpuretoBranchNotDeleted branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not deleted"
    pretty TenpuretoEmptySelection =
        "Cannot create a project from an empty selection"
    pretty TenpuretoEmptyChangeset =
        "Cannot create a commit because the changeset is empty"
    pretty (TenpuretoGitException e) = pretty e
    pretty (TenpuretoUIException  e) = pretty e
    pretty (MultipleExceptions ee) =
        "Multiple failures:\n" <> (align . vsep) (fmap pretty ee)
    pretty CancelledException = "Cancelled by a user"

netlistGraph'
    :: (Ord a)
    => (b -> [(String, String)])
    -> (b -> b -> [(String, String)])
    -> (b -> [a])
    -> [(a, b)]
    -> Dot ()
netlistGraph' attrFn edgeAttrFn outFn assocs = do
    let assocsMap = Map.fromList assocs
    let nodes     = Set.fromList $ [ a | (a, _) <- assocs ]
    let outs = Set.fromList $ [ o | (_, b) <- assocs, o <- outFn b ]
    nodeTab  <- sequence [ (a, ) <$> node (attrFn b) | (a, b) <- assocs ]
    otherTab <- sequence
        [ (o, ) <$> node [] | o <- Set.toList outs, o `Set.notMember` nodes ]
    let fm = Map.fromList (nodeTab ++ otherTab)
    sequence_
        [ edge (fm Map.! src) (fm Map.! dst) (fromMaybe [] edgeAttr)
        | (dst, b) <- assocs
        , src      <- outFn b
        , let edgeAttr = edgeAttrFn <$> Map.lookup src assocsMap <*> pure b
        ]
    return ()
