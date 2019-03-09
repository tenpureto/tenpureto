{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Console
import qualified Console.Byline                as B
import           Git
import qualified Git.Cli                       as GC
import           Data
import           Tenpureto

newtype AppM a = AppM { unAppM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runAppM :: AppM a -> IO a
runAppM = unAppM

instance MonadGit AppM where
    withClonedRepository = GC.withClonedRepository
    listBranches         = GC.listBranches

instance MonadConsole AppM where
    ask = B.ask
    askUntil a b c = B.askUntil a b (return . c)
    sayLn = B.sayLn

data Command
    = Create
            { templateName :: Maybe String
            , runUnattended :: Bool
            }
    | Update
            { maybeTemplateName :: Maybe String
            , runUnattended :: Bool
            }

template :: Parser String
template = strOption
    (long "template" <> metavar "<repository>" <> help
        "Template repository name or URL"
    )

unattended :: Parser Bool
unattended = switch (long "unattended" <> help "Do not ask anything")

create :: Parser Command
create = Create <$> optional template <*> unattended

update :: Parser Command
update = Update <$> optional template <*> unattended

run :: Command -> AppM ()
run Create { templateName = t, runUnattended = u } = createProject
    PreliminaryProjectConfiguration { preSelectedTemplate = t
                                    , preSelectedBranches = Nothing
                                    , preVariableValues   = Nothing
                                    }
    u
run Update { maybeTemplateName = t, runUnattended = u } = updateProject
    PreliminaryProjectConfiguration { preSelectedTemplate = t
                                    , preSelectedBranches = Nothing
                                    , preVariableValues   = Nothing
                                    }
    u

main :: IO ()
main = runAppM . run =<< customExecParser p opts
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
