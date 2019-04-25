{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Maybe
import           Data.Text                      ( Text )
import           Console
import qualified Console.Terminal              as Terminal
import           Git
import qualified Git.Cli                       as GC
import           Data
import           Logging
import           Tenpureto
import           UI                             ( resolveTargetDir )

newtype AppM a = AppM { unAppM :: LoggingT IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLog)

runAppM :: Bool -> AppM a -> IO a
runAppM withDebug app =
    initPrintToTerminal withDebug >>= runLoggingT (unAppM app)

instance MonadGit AppM where
    withClonedRepository    = GC.withClonedRepository
    withRepository          = GC.withRepository
    withNewWorktree         = GC.withNewWorktree
    initRepository          = GC.initRepository
    listBranches            = GC.listBranches
    checkoutBranch          = GC.checkoutBranch
    mergeBranch             = GC.mergeBranch
    runMergeTool            = GC.runMergeTool
    getBranchFile           = GC.getBranchFile
    getWorkingCopyFile      = GC.getWorkingCopyFile
    writeAddFile            = GC.writeAddFile
    addFiles                = GC.addFiles
    commit                  = GC.commit
    findCommit              = GC.findCommit
    getCommitMessage        = GC.getCommitMessage
    listFiles               = GC.listFiles
    populateRerereFromMerge = GC.populateRerereFromMerge

instance MonadConsole AppM where
    ask      = Terminal.ask
    askUntil = Terminal.askUntil
    sayLn    = Terminal.sayLn

data Command
    = Create
            { templateName :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | Update
            { maybeTemplateName :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }

templateNameOption :: Parser Text
templateNameOption = strOption
    (long "template" <> metavar "<repository>" <> help
        "Template repository name or URL"
    )

targetArgument :: Parser FilePath
targetArgument = strArgument (metavar "<directory>" <> help "Target directory")

unattendedSwitch :: Parser Bool
unattendedSwitch = switch (long "unattended" <> help "Do not ask anything")

debugSwitch :: Parser Bool
debugSwitch = switch (long "debug" <> help "Print debug information")

createCommand :: Parser Command
createCommand =
    Create
        <$> optional templateNameOption
        <*> optional targetArgument
        <*> unattendedSwitch
        <*> debugSwitch

updateCommand :: Parser Command
updateCommand =
    Update
        <$> optional templateNameOption
        <*> optional targetArgument
        <*> unattendedSwitch
        <*> debugSwitch

run :: Command -> IO ()
run Create { templateName = t, maybeTargetDirectory = td, runUnattended = u, enableDebugLogging = d }
    = runAppM d $ do
        resolvedTd <- traverse resolveTargetDir td
        createProject
            PreliminaryProjectConfiguration
                { preSelectedTemplate       = t
                , preTargetDirectory        = resolvedTd
                , prePreviousTemplateCommit = Nothing
                , preSelectedBranches       = Nothing
                , preVariableValues         = Nothing
                }
            u
run Update { maybeTemplateName = t, maybeTargetDirectory = td, runUnattended = u, enableDebugLogging = d }
    = runAppM d $ do
        resolvedTd    <- resolveTargetDir (fromMaybe "." td)
        currentConfig <- loadExistingProjectConfiguration resolvedTd
        let inputConfig = PreliminaryProjectConfiguration
                { preSelectedTemplate       = t
                , preTargetDirectory        = Just resolvedTd
                , prePreviousTemplateCommit = Nothing
                , preSelectedBranches       = Nothing
                , preVariableValues         = Nothing
                }
        updateProject (inputConfig <> currentConfig) u

main :: IO ()
main = run =<< customExecParser p opts
  where
    commands = subparser
        (  command
                "create"
                (info (createCommand <**> helper)
                      (progDesc "Create a new project for a template")
                )
        <> command
               "update"
               (info (updateCommand <**> helper)
                     (progDesc "Update a project for a template")
               )
        )
    opts = info (commands <**> helper) fullDesc
    p    = prefs showHelpOnEmpty
