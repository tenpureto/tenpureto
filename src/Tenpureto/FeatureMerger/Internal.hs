module Tenpureto.FeatureMerger.Internal where

import           Data.Text                      ( Text )
import           Data.Map                       ( Map )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Tenpureto.Effects.Git

data Tree a = Leaf a
            | Node a [Tree a]
            deriving (Eq, Ord)

showTree :: Tree Text -> Text
showTree a =
    renderStrict $ layoutPretty (LayoutOptions Unbounded) $ showTree' a

showTree' :: Tree Text -> Doc ()
showTree' (Leaf a) = pretty a
showTree' (Node a children) =
    pretty a <> indent 2 (vsep (fmap showTree' children))

type MergeCacheElement = (Committish, Text, Tree Text)
type MergeCacheKey = (MergeCacheElement, MergeCacheElement)
type MergeCache = Map MergeCacheKey MergeCacheElement
