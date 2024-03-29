module Tenpureto.FeatureMerger.Internal where

import           Data.Map                       ( Map )
import           Prettyprinter
import           Prettyprinter.Render.Text

import           Tenpureto.Effects.Git
import           Tenpureto.OrderedSet           ( OrderedSet )

data Tree a = Leaf a
            | Node a [Tree a]
            deriving (Eq, Ord)

data MergeElement = MergeElement
    { mergeHead  :: Committish
    , mergeName  :: Text
    , mergeTree  :: Tree Text
    , mergeHeads :: OrderedSet Committish
    }
    deriving (Eq, Ord)

showTree :: Tree Text -> Text
showTree a =
    renderStrict $ layoutPretty (LayoutOptions Unbounded) $ showTree' a

showTree' :: Tree Text -> Doc ()
showTree' (Leaf a) = pretty a
showTree' (Node a children) =
    pretty a <> "\n" <> indent 2 (vsep (fmap showTree' children))

type MergeCacheKey = (MergeElement, MergeElement)
type MergeCache = Map MergeCacheKey MergeElement
