{-# LANGUAGE OverloadedStrings #-}

module Console.Terminal where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           System.IO                      ( stdout
                                                , hFlush
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Functor
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified System.Console.Terminal.Size  as TS

data InterruptedException = InterruptedException
    deriving Show

instance Exception InterruptedException

ask :: MonadIO m => Doc AnsiStyle -> Maybe Text -> m Text
ask prompt (Just defans) = do
    say $ prompt <+> annotate (color Black) ((brackets . pretty) defans) <> " "
    input <- liftIO $ T.pack <$> getLine
    return $ if T.null input then defans else input
ask prompt Nothing = do
    say $ prompt <> " "
    liftIO $ T.pack <$> getLine

askUntil
    :: MonadIO m
    => Doc AnsiStyle
    -> Maybe Text
    -> (Text -> Either (Doc AnsiStyle) a)
    -> m a
askUntil prompt defans confirm = ask prompt defans <&> confirm >>= reprompt
  where
    reprompt (Left  prompt') = askUntil (prompt' <+> prompt) defans confirm
    reprompt (Right result ) = return result

say :: MonadIO m => Doc AnsiStyle -> m ()
say message = liftIO $ do
    w <- fmap TS.width <$> TS.size
    let layoutOptions =
            LayoutOptions (maybe Unbounded (flip AvailablePerLine 1.0) w)
        render = renderIO stdout . layoutPretty layoutOptions
    render message
    hFlush stdout

sayLn :: MonadIO m => Doc AnsiStyle -> m ()
sayLn message = say (message <> "\n")
