{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tenpureto.Orphanage where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Text.Prettyprint.Doc
import           Data.YAML                      ( FromYAML(..)
                                                , ToYAML(..)
                                                )
import           Path

instance Pretty (Path a t) where
    pretty = pretty . toFilePath

instance Pretty a => Pretty (Set a) where
    pretty s = group . encloseSep "[ " " ]" ", " $ pretty <$> Set.toList s

instance (Pretty a, Pretty b) => Pretty (Map a b) where
    pretty s = group . encloseSep "{ " " }" ", " $ pretty <$> Map.toList s

instance (Pretty a, Pretty b) => Pretty (InsOrdHashMap a b) where
    pretty s =
        group . encloseSep "{ " " }" ", " $ pretty <$> InsOrdHashMap.toList s

instance (Ord a, FromYAML a) => FromYAML (Set a) where
    parseYAML x = Set.fromList <$> parseYAML x

instance ToYAML a => ToYAML (Set a) where
    toYAML = toYAML . Set.toList
