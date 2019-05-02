{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Version
import           Console
import qualified Console.Terminal              as Terminal
import           Git
import qualified Git.Cli                       as GC
import           Data
import           Logging
import           Tenpureto
import           UI                             ( resolveTargetDir )
import           Paths_tenpureto                ( version )

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
    getCommitContent        = GC.getCommitContent
    listFiles               = GC.listFiles
    populateRerereFromMerge = GC.populateRerereFromMerge
    getCurrentBranch        = GC.getCurrentBranch
    getCurrentHead          = GC.getCurrentHead
    renameCurrentBranch     = GC.renameCurrentBranch
    pushRefs                = GC.pushRefs

instance MonadConsole AppM where
    ask      = Terminal.ask
    askUntil = Terminal.askUntil
    sayLn    = Terminal.sayLn

data Command
    = Create
            { maybeTemplateName :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | Update
            { maybeTemplateName :: Maybe Text
            , maybePreviousCommit :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | TemplateRenameBranch
            { templateName :: Text
            , oldBranchName :: Text
            , newBranchName :: Text
            , enableInteractivity :: Bool
            , enableDebugLogging :: Bool
            }
    | TemplateChangeVariable
            { templateName :: Text
            , oldVariableValue :: Text
            , newVariableValue :: Text
            , enableInteractivity :: Bool
            , enableDebugLogging :: Bool
            }

templateNameOption :: Parser Text
templateNameOption = strOption
    (long "template" <> metavar "<repository>" <> help
        "Template repository name or URL"
    )

previousCommitOption :: Parser Text
previousCommitOption = strOption
    (long "previous-commit" <> metavar "<sha1>" <> help
        "Override the commit in which the template was previously generated"
    )

targetArgument :: Parser FilePath
targetArgument = strArgument (metavar "<dir>" <> help "Target directory")

unattendedSwitch :: Parser Bool
unattendedSwitch = switch (long "unattended" <> help "Do not ask anything")

interactiveSwitch :: Parser Bool
interactiveSwitch = switch
    (  long "interactive"
    <> help "Stop before committing to allow manual intervention"
    )

debugSwitch :: Parser Bool
debugSwitch = switch (long "debug" <> help "Print debug information")

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("tenpureto " ++ showVersion version)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)

oldBranchNameOption :: Parser Text
oldBranchNameOption =
    strOption (long "old-name" <> metavar "<branch>" <> help "Old branch name")

newBranchNameOption :: Parser Text
newBranchNameOption =
    strOption (long "new-name" <> metavar "<branch>" <> help "New branch name")

oldVariableValueOption :: Parser Text
oldVariableValueOption = strOption
    (long "old-value" <> metavar "<value>" <> help "Old variable value")

newVariableValueOption :: Parser Text
newVariableValueOption = strOption
    (long "new-value" <> metavar "<value>" <> help "New variable value")

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
        <*> optional previousCommitOption
        <*> optional targetArgument
        <*> unattendedSwitch
        <*> debugSwitch

renameBranchCommand :: Parser Command
renameBranchCommand =
    TemplateRenameBranch
        <$> templateNameOption
        <*> oldBranchNameOption
        <*> newBranchNameOption
        <*> interactiveSwitch
        <*> debugSwitch

changeVariableCommand :: Parser Command
changeVariableCommand =
    TemplateChangeVariable
        <$> templateNameOption
        <*> oldVariableValueOption
        <*> newVariableValueOption
        <*> interactiveSwitch
        <*> debugSwitch

templateCommands :: Parser Command
templateCommands = hsubparser
    (  command
            "rename-branch"
            (info renameBranchCommand (progDesc "Rename a template branch"))
    <> command
           "change-variable"
           (info changeVariableCommand
                 (progDesc "Change a template variable value")
           )
    )

run :: Command -> IO ()
run Create { maybeTemplateName = t, maybeTargetDirectory = td, runUnattended = u, enableDebugLogging = d }
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
run Update { maybeTemplateName = t, maybeTargetDirectory = td, maybePreviousCommit = pc, runUnattended = u, enableDebugLogging = d }
    = runAppM d $ do
        resolvedTd    <- resolveTargetDir (fromMaybe "." td)
        currentConfig <- loadExistingProjectConfiguration resolvedTd
        let inputConfig = PreliminaryProjectConfiguration
                { preSelectedTemplate       = t
                , preTargetDirectory        = Just resolvedTd
                , prePreviousTemplateCommit = fmap Committish pc
                , preSelectedBranches       = Nothing
                , preVariableValues         = Nothing
                }
        updateProject (inputConfig <> currentConfig) u
run TemplateRenameBranch { templateName = t, oldBranchName = on, newBranchName = nn, enableInteractivity = i, enableDebugLogging = d }
    = runAppM d $ renameTemplateBranch t on nn i
run TemplateChangeVariable { templateName = t, oldVariableValue = ov, newVariableValue = nv, enableInteractivity = i, enableDebugLogging = d }
    = runAppM d $ changeTemplateVariableValue t ov nv i

main :: IO ()
main = run =<< customExecParser p opts
  where
    commands = hsubparser
        (  command
              "create"
              (info createCommand
                    (progDesc "Create a new project for a template")
              )
        <> command
               "update"
               (info updateCommand
                     (progDesc "Update a project for a template")
               )
        <> command "template"
                   (info templateCommands (progDesc "Manage a template"))
        )
    opts = info (commands <**> versionOption <**> helper) fullDesc
    p    = prefs showHelpOnEmpty
