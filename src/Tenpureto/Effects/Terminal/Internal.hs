{-# LANGUAGE TupleSections #-}

module Tenpureto.Effects.Terminal.Internal where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Functor

import           System.IO                      ( stdout
                                                , hFlush
                                                )
import           System.Console.ANSI            ( cursorUp
                                                , setCursorColumn
                                                , clearFromCursorToScreenEnd
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

putDocTerminal :: Doc AnsiStyle -> IO Int
putDocTerminal msg = do
    options <- layoutOptions <$> getTerminalWidth
    let text = renderStrict (layoutPretty options msg)
    let h = length (T.lines text)
    putStr (T.unpack text)
    hFlush stdout
    return h

askTerminalH :: Doc AnsiStyle -> Maybe Text -> IO (Text, Int)
askTerminalH prompt (Just defans) = do
    h <- putDocTerminal
        $   prompt
        <+> annotate (color Black) ((brackets . pretty) defans)
        <>  " "
    input <- T.pack <$> getLine
    return (if T.null input then defans else input) <&> (, h)
askTerminalH prompt Nothing = do
    h <- putDocTerminal $ prompt <> " "
    T.pack <$> getLine <&> (, h)

askTerminal :: Doc AnsiStyle -> Maybe Text -> IO Text
askTerminal prompt defans = askTerminalH prompt defans <&> fst

askTerminalUntil
    :: s
    -> (s -> (Doc AnsiStyle, Maybe Text))
    -> (s -> Text -> Either s a)
    -> IO a
askTerminalUntil state request process = do
    (ans, h) <- uncurry askTerminalH (request state)
    reprompt h (process state ans)
  where
    reprompt h (Left s) = do
        cursorUp h
        setCursorColumn 0
        clearFromCursorToScreenEnd
        askTerminalUntil s request process
    reprompt _ (Right r) = return r
