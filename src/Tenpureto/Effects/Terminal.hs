{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.Terminal
    ( module Tenpureto.Effects.Terminal
    , module Data.Text.Prettyprint.Doc
    , Text
    , AnsiStyle
    )
where

import           Polysemy

import           Data.Bool
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Tenpureto.Effects.Terminal.Internal

data Terminal m a where
    TerminalWidth ::Terminal m (Maybe Int)
    SayLn ::Doc AnsiStyle -> Terminal m ()

data TerminalInput m a where
    Ask ::Doc AnsiStyle -> Maybe Text -> TerminalInput m Text
    AskUntil ::s -> (s -> (Doc AnsiStyle, Maybe Text)) -> (s -> Text -> Either s a) -> TerminalInput m a

makeSem ''Terminal
makeSem ''TerminalInput


confirm :: Member TerminalInput r => Doc AnsiStyle -> Maybe Bool -> Sem r Bool
confirm msg def = askUntil Nothing request process
  where
    defAns = fmap (bool "n" "y") def
    request Nothing = (msg <+> "(y/n)?", defAns)
    request (Just _) =
        ("Please answer \"y\" or \"n\"." <+> msg <+> "(y/n)?", defAns)
    process = const mapAnswer
    mapAnswer "y" = Right True
    mapAnswer "n" = Right False
    mapAnswer p   = Left (Just p)



runTerminalIO
    :: Member (Lift IO) r => Sem (TerminalInput ': Terminal ': r) a -> Sem r a
runTerminalIO = runTerminalIOOutput . runTerminalIOInput
  where
    runTerminalIOOutput
        :: Member (Lift IO) r => Sem (Terminal ': r) a -> Sem r a
    runTerminalIOOutput = interpret $ \case
        TerminalWidth -> sendM getTerminalWidth
        SayLn msg     -> sendM $ sayLnTerminal msg
    runTerminalIOInput
        :: Member (Lift IO) r => Sem (TerminalInput ': r) a -> Sem r a
    runTerminalIOInput = interpret $ \case
        Ask msg defans -> sendM $ askTerminal msg defans
        AskUntil state request process ->
            sendM $ askTerminalUntil state request process
