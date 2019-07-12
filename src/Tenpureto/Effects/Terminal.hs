{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Tenpureto.Effects.Terminal
    ( module Tenpureto.Effects.Terminal
    , module Data.Text.Prettyprint.Doc
    , Text
    , AnsiStyle
    )
where

import           Polysemy
import           Polysemy.State
import           Polysemy.Resource
import           Polysemy.IdempotentLowering

import           Data.Bool
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.IORef

import           Tenpureto.Effects.Terminal.Internal

data Terminal m a where
    TerminalWidth ::Terminal m (Maybe Int)
    SayLn ::Doc AnsiStyle -> Terminal m ()
    SayLnTemporary ::Doc AnsiStyle -> Terminal m ()

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

traverseWithProgressBar
    :: (Members '[Terminal, Resource] r, Traversable t)
    => (a -> Doc AnsiStyle)
    -> (a -> Sem r b)
    -> t a
    -> Sem r (t b)
traverseWithProgressBar info action tasks =
    let total = length tasks
        percentage idx = case idx * 100 `div` total of
            x | x < 10 -> space <> pretty x
            x          -> pretty x
        fullInfo idx a = brackets (percentage idx <> "%") <+> info a
        showInfo idx a = sayLnTemporary $ fullInfo idx a
        fullAction idx a = showInfo idx a >> action a
    in  traverseWithIndex fullAction tasks

runTerminalIOOutput
    :: Member (Lift IO) r
    => (forall x . Sem r x -> IO x)
    -> IO (forall a . Sem (Terminal ': r) a -> Sem r a)
runTerminalIOOutput _ = do
    ioRef <- newIORef (TemporaryHeight 0)
    nat $ runStateInIORef ioRef . reinterpret \case
        TerminalWidth -> sendM getTerminalWidth
        SayLn msg     -> do
            TemporaryHeight ph <- get
            sendM $ clearLastLinesTerminal ph
            put $ TemporaryHeight 0
            sendM $ sayLnTerminal msg
        SayLnTemporary msg -> do
            TemporaryHeight ph <- get
            sendM $ clearLastLinesTerminal ph
            h <- sendM $ sayLnTerminal' msg
            put $ TemporaryHeight h

runTerminalIOInput
    :: Member (Lift IO) r => Sem (TerminalInput ': r) a -> Sem r a
runTerminalIOInput = interpret $ \case
    Ask msg defans -> sendM $ askTerminal msg defans
    AskUntil state request process ->
        sendM $ askTerminalUntil state request process
