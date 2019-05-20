{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tenpureto.Exec where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           System.Exit
import           System.Process.Typed

import           Logging

data ExecException = ExecException
    { exitCode :: Int
    , stdErr :: Text }
    deriving Show

instance Exception ExecException

prefixed :: Doc () -> Text -> Doc ()
prefixed prefix =
    concatWith (\a b -> a <> hardline <> b)
        . map ((<>) prefix . pretty)
        . T.lines

decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8 . BS.toStrict

stdoutOrThrow
    :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ByteString
stdoutOrThrow (ExitSuccess, out, err) = return out
stdoutOrThrow (ExitFailure code, out, err) =
    throwM $ ExecException code (decodeUtf8 err)

maybeStdout :: (ExitCode, ByteString, ByteString) -> Maybe ByteString
maybeStdout (ExitSuccess     , out, err) = Just out
maybeStdout (ExitFailure code, out, err) = Nothing

unitOrThrow :: MonadThrow m => (ExitCode, ByteString, ByteString) -> m ()
unitOrThrow a = void (stdoutOrThrow a)

throwIfFailed :: MonadThrow m => ExitCode -> m ()
throwIfFailed ExitSuccess        = return ()
throwIfFailed (ExitFailure code) = throwM $ ExecException code ""

runCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => NonEmpty Text
    -> m (ExitCode, ByteString, ByteString)
runCmd (cmd :| args) = do
    logInfo $ "Running" <+> pretty cmd <+> fillSep (map pretty args)
    let process = setStdin closed $ proc (T.unpack cmd) (map T.unpack args)
    (exitCode, out, err) <- liftIO $ readProcess process
    logDebug $ prefixed "err> " (decodeUtf8 err)
    logDebug $ prefixed "out> " (decodeUtf8 out)
    return (exitCode, out, err)

runInteractiveCmd
    :: (MonadIO m, MonadThrow m, MonadLog m) => NonEmpty Text -> m ExitCode
runInteractiveCmd (cmd :| args) = do
    logInfo $ "Running interactively" <+> pretty cmd <+> fillSep
        (map pretty args)
    let process =
            setStdin inherit $ setStdout inherit $ setStderr inherit $ proc
                (T.unpack cmd)
                (map T.unpack args)
    liftIO $ runProcess process

runInputCmd
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => NonEmpty Text
    -> ByteString
    -> m (ExitCode, ByteString, ByteString)
runInputCmd (cmd :| args) input = do
    logInfo $ "Running" <+> pretty cmd <+> fillSep (map pretty args)
    logDebug $ prefixed "in> " (decodeUtf8 input)
    let process = setStdin (byteStringInput input)
            $ proc (T.unpack cmd) (map T.unpack args)
    (exitCode, out, err) <- liftIO $ readProcess process
    logDebug $ prefixed "err> " (decodeUtf8 err)
    logDebug $ prefixed "out> " (decodeUtf8 out)
    return (exitCode, out, err)
