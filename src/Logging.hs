{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Logging
    ( module Logging
    , module Control.Monad.Log
    , module Data.Text.Prettyprint.Doc
    )
where

import           Control.Monad.Log       hiding ( MonadLog
                                                , LoggingT
                                                )
import qualified Control.Monad.Log             as L
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
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

instance Pretty a => Pretty (Set a) where
    pretty s = group . encloseSep "[ " " ]" ", " $ pretty <$> Set.toList s

instance (Pretty a, Pretty b) => Pretty (Map a b) where
    pretty s = group . encloseSep "{ " " }" ", " $ pretty <$> Map.toList s

instance (Pretty a, Pretty b) => Pretty (InsOrdHashMap a b) where
    pretty s = group . encloseSep "{ " " }" ", " $ pretty <$> InsOrdHashMap.toList s
