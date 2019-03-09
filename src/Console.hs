module Console
  ( module Console
  , module System.Console.Byline.Stylized
  , module System.Console.Byline.Modifiers
  , module System.Console.Byline.Color
  )
where

import           Data.Text                      ( Text )
import           System.Console.Byline.Stylized
import           System.Console.Byline.Modifiers
import           System.Console.Byline.Color

class Monad m => MonadConsole m where
  ask :: Stylized -> Maybe Text -> m Text
  askUntil :: Stylized -> Maybe Text -> (Text -> Either Stylized Text) -> m Text
  sayLn :: Stylized -> m ()
