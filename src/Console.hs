module Console
    ( module Console
    , module Data.Text.Prettyprint.Doc
    , module Data.Text.Prettyprint.Doc.Render.Terminal
    )
where

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

class Monad m => MonadConsole m where
  ask :: Doc AnsiStyle -> Maybe Text -> m Text
  askUntil :: Doc AnsiStyle -> Maybe Text -> (Text -> Either (Doc AnsiStyle) a) -> m a
  sayLn :: Doc AnsiStyle -> m ()
