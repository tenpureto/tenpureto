{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Logging
    ( module Logging
    , module Control.Monad.Log
    , module Data.Text.Prettyprint.Doc
    )
where

import           Data.Maybe
import           Control.Monad.Log       hiding ( MonadLog
                                                , LoggingT
                                                )
import qualified Control.Monad.Log             as L
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           System.IO
import qualified System.Console.Terminal.Size  as TS
import           Path

type LogMessage = L.WithSeverity (Doc ())
type MonadLog = L.MonadLog LogMessage
type LoggingT = L.LoggingT LogMessage

initPrintToTerminal :: Bool -> IO (LogMessage -> IO ())
initPrintToTerminal False = return $ const (return ())
initPrintToTerminal True  = printToTerminal . fmap TS.width <$> TS.size

printToTerminal :: Maybe Int -> LogMessage -> IO ()
printToTerminal width message =
    let layoutOptions = LayoutOptions
            (maybe Unbounded (flip AvailablePerLine 1.0) width)
        text        = discardSeverity message <> "\n"
        style       = color Black
        reannotated = reAnnotate (const mempty) text
        annotated   = annotate style reannotated
        sdoc        = layoutSmart layoutOptions annotated
    in  renderIO stdout sdoc

instance Pretty (Path a t) where
    pretty = pretty . toFilePath
