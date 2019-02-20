{-# LANGUAGE DeriveAnyClass #-}

module UI
    ( module UI
    , MonadException
    )
where

import           Data
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           System.Console.Haskeline

data UIException = UnattendedNotPossible  deriving (Exception)

instance Show UIException where
    show UnattendedNotPossible =
        "Running in an unattended mode, but some input required"

unattendedConfiguration
    :: (MonadThrow m)
    => PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
unattendedConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Just t, preSelectedBranches = b, preVariableValues = v }
    = return FinalProjectConfiguration { selectedTemplate = t
                                       , selectedBranches = b
                                       , variableValues   = v
                                       }
unattendedConfiguration _ = throwM UnattendedNotPossible

required :: (Monad m) => m (Maybe a) -> m a
required input = input >>= maybe (required input) return

inputTemplate :: (MonadException m) => InputT m String
inputTemplate = required . getInputLine $ "Template URL: "

inputConfiguration
    :: (MonadIO m, MonadException m)
    => PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
inputConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt, preSelectedBranches = b, preVariableValues = v }
    = runInputT defaultSettings $ do
        t <- case mbt of
            Just t  -> return t
            Nothing -> inputTemplate
        return FinalProjectConfiguration { selectedTemplate = t
                                         , selectedBranches = b
                                         , variableValues   = v
                                         }
