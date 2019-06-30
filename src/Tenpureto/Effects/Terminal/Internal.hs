module Tenpureto.Effects.Terminal.Internal where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Functor

import           System.IO                      ( stdout
                                                , hFlush
                                                )
import qualified System.Console.Terminal.Size  as TS

layoutOptions :: Maybe Int -> LayoutOptions
layoutOptions w = LayoutOptions $ maybe Unbounded (flip AvailablePerLine 1.0) w

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = fmap TS.width <$> TS.size

renderToTerminal :: Doc AnsiStyle -> IO ()
renderToTerminal msg = do
    options <- layoutOptions <$> getTerminalWidth
    renderIO stdout (layoutPretty options msg)
    hFlush stdout

sayLnTerminal :: Doc AnsiStyle -> IO ()
sayLnTerminal msg = renderToTerminal (msg <> "\n")

askTerminal :: Doc AnsiStyle -> Maybe Text -> IO Text
askTerminal prompt (Just defans) = do
    renderToTerminal
        $   prompt
        <+> annotate (color Black) ((brackets . pretty) defans)
        <>  " "
    input <- T.pack <$> getLine
    return $ if T.null input then defans else input
askTerminal prompt Nothing = do
    renderToTerminal $ prompt <> " "
    T.pack <$> getLine

askTerminalUntil
    :: Doc AnsiStyle -> Maybe Text -> (Text -> Either (Doc AnsiStyle) a) -> IO a
askTerminalUntil prompt defans confirm =
    askTerminal prompt defans <&> confirm >>= reprompt
  where
    reprompt (Left prompt') =
        askTerminalUntil (prompt' <+> prompt) defans confirm
    reprompt (Right result) = return result
