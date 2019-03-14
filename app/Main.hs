{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Console
import qualified Console.Byline                as B
import           Git
import qualified Git.Cli                       as GC
import           Data
import           Logging
import           Tenpureto

newtype AppM a = AppM { unAppM :: LoggingT IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLog)

runAppM :: Bool -> AppM a -> IO a
runAppM debug app = initPrintToTerminal debug >>= runLoggingT (unAppM app)

instance MonadGit AppM where
    withClonedRepository = GC.withClonedRepository
    listBranches         = GC.listBranches
    checkoutBranch       = GC.checkoutBranch
    mergeBranch          = GC.mergeBranch
    runMergeTool         = GC.runMergeTool
    getBranchFile        = GC.getBranchFile
    addFile              = GC.addFile

instance MonadConsole AppM where
    ask = B.ask
    askUntil a b c = B.askUntil a b (return . c)
    sayLn = B.sayLn

data Command
    = Create
            { templateName :: Maybe Text
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | Update
            { maybeTemplateName :: Maybe Text
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }

template :: Parser Text
template = strOption
    (long "template" <> metavar "<repository>" <> help
        "Template repository name or URL"
    )

unattended :: Parser Bool
unattended = switch (long "unattended" <> help "Do not ask anything")

debug :: Parser Bool
debug = switch (long "debug" <> help "Print debug information")

create :: Parser Command
create = Create <$> optional template <*> unattended <*> debug

update :: Parser Command
update = Update <$> optional template <*> unattended <*> debug

run :: Command -> IO ()
run Create { templateName = t, runUnattended = u, enableDebugLogging = d } =
    runAppM d $ createProject
        PreliminaryProjectConfiguration { preSelectedTemplate        = t
                                        , preSelectedBaseBranch      = Nothing
                                        , preSelectedFeatureBranches = Nothing
                                        , preVariableValues          = Nothing
                                        }
        u
run Update { maybeTemplateName = t, runUnattended = u, enableDebugLogging = d }
    = runAppM d $ updateProject
        PreliminaryProjectConfiguration { preSelectedTemplate        = t
                                        , preSelectedBaseBranch      = Nothing
                                        , preSelectedFeatureBranches = Nothing
                                        , preVariableValues          = Nothing
                                        }
        u

main :: IO ()
main = run =<< customExecParser p opts
  where
    commands = subparser
        (  command
                "create"
                (info (create <**> helper)
                      (progDesc "Create a new project for a template")
                )
        <> command
               "update"
               (info (update <**> helper)
                     (progDesc "Update a project for a template")
               )
        )
    opts = info (commands <**> helper) fullDesc
    p    = prefs showHelpOnEmpty
