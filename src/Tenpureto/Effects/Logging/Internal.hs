module Tenpureto.Effects.Logging.Internal where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

data Severity = Silent | Info | Debug
    deriving (Eq, Ord)

formatTerminalMessage :: Severity -> Doc () -> Doc AnsiStyle
formatTerminalMessage _ = annotate (color Black) . reAnnotate (const mempty)
