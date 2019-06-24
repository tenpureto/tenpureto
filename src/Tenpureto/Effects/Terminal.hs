{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.Terminal
    ( module Tenpureto.Effects.Terminal
    , module Data.Text.Prettyprint.Doc
    , Text
    , AnsiStyle
    )
where

import           Polysemy

import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Tenpureto.Effects.Terminal.Internal

data Terminal m a where
    SayLn ::Doc AnsiStyle -> Terminal m ()

data TerminalInput m a where
    Ask ::Doc AnsiStyle -> Maybe Text -> TerminalInput m Text
    AskUntil ::Doc AnsiStyle -> Maybe Text -> (Text -> Either (Doc AnsiStyle) a) -> TerminalInput m a

makeSem ''Terminal
makeSem ''TerminalInput


confirm :: Member TerminalInput r => Doc AnsiStyle -> Maybe Bool -> Sem r Bool
confirm request def = askUntil (request <+> "(y/n)?")
                               (fmap defAns def)
                               mapAnswer
  where
    mapAnswer x = case x of
        "y" -> Right True
        "n" -> Right False
        _   -> Left "Please answer \"y\" or \"n\"."
    defAns True  = "y"
    defAns False = "n"



runTerminalIO
    :: Member (Lift IO) r => Sem (TerminalInput ': Terminal ': r) a -> Sem r a
runTerminalIO = runTerminalIOOutput . runTerminalIOInput
  where
    runTerminalIOOutput
        :: Member (Lift IO) r => Sem (Terminal ': r) a -> Sem r a
    runTerminalIOOutput = interpret $ \case
        SayLn msg -> sendM $ sayLnTerminal msg
    runTerminalIOInput
        :: Member (Lift IO) r => Sem (TerminalInput ': r) a -> Sem r a
    runTerminalIOInput = interpret $ \case
        Ask msg defans            -> sendM $ askTerminal msg defans
        AskUntil msg defans reask -> sendM $ askTerminalUntil msg defans reask
