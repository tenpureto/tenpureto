{-# LANGUAGE DeriveAnyClass #-}

module UI
    ( module UI
    , MonadException
    )
where

import           Data
import           Control.Applicative
import           Control.Monad
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
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
unattendedProjectConfiguration _ providedConfiguration currentConfiguration =
    let b =
                preSelectedBranches providedConfiguration
                    `mplus` fmap selectedBranches currentConfiguration
        v =
                preVariableValues providedConfiguration
                    `mplus` fmap variableValues currentConfiguration
        cfg = FinalProjectConfiguration <$> b <*> v
    in  maybe (throwM UnattendedNotPossible) return cfg

required :: (Monad m) => m (Maybe a) -> m a
required input = input >>= maybe (required input) return

inputTemplate :: (MonadException m) => InputT m String
inputTemplate = required . getInputLine $ "Template URL: "


inputTemplateConfiguration
    :: (MonadIO m, MonadException m)
    => PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
inputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt }
    = runInputT defaultSettings
        $   FinalTemplateConfiguration
        <$> maybe inputTemplate return mbt

inputProjectConfiguration
    :: (MonadIO m, MonadException m)
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Maybe FinalProjectConfiguration
    -> m FinalProjectConfiguration
inputProjectConfiguration _ providedConfiguration currentConfiguration =
    return FinalProjectConfiguration { selectedBranches = []
                                     , variableValues   = []
                                     }
