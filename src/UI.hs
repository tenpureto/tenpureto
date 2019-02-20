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

unattendedTemplateConfiguration
    :: (MonadThrow m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
unattendedTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Just t }
    = return FinalTemplateConfiguration { selectedTemplate = t }
unattendedTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = Nothing }
    = throwM UnattendedNotPossible

unattendedProjectConfiguration
    :: (MonadThrow m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
unattendedProjectConfiguration _ PreliminaryProjectConfiguration { preSelectedBranches = b, preVariableValues = v }
    = return FinalProjectConfiguration { selectedBranches = b
                                       , variableValues   = v
                                       }

required :: (Monad m) => m (Maybe a) -> m a
required input = input >>= maybe (required input) return

inputTemplate :: (MonadException m) => InputT m String
inputTemplate = required . getInputLine $ "Template URL: "


inputTemplateConfiguration
    :: (MonadIO m, MonadException m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
inputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt }
    = runInputT defaultSettings $ do
        t <- case mbt of
            Just t  -> return t
            Nothing -> inputTemplate
        return FinalTemplateConfiguration { selectedTemplate = t }

inputProjectConfiguration
    :: (MonadIO m, MonadException m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
inputProjectConfiguration _ PreliminaryProjectConfiguration { preSelectedBranches = b, preVariableValues = v }
    = return FinalProjectConfiguration { selectedBranches = b
                                       , variableValues   = v
                                       }
