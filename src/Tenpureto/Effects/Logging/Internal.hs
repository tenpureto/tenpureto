module Tenpureto.Effects.Logging.Internal where

import           Prettyprinter

import           Prettyprinter.Render.Terminal

data Severity = Silent | Info | Debug
    deriving (Eq, Ord)

formatTerminalMessage :: Severity -> Doc () -> Doc AnsiStyle
formatTerminalMessage _ = annotate (color Black) . reAnnotate (const mempty)
