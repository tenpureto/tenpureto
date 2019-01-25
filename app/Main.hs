module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )


data Command
    = Create
            { templateName :: String
            }
    | Update
            { maybeTemplateName :: Maybe String
            }

template :: Parser String
template = strOption
    (long "template" <> metavar "<repository>" <> help
        "Template repository name or URL"
    )

create :: Parser Command
create = Create <$> template

update :: Parser Command
update = Update <$> (optional template)

run :: Command -> IO ()
run Create { templateName = t }      = putStrLn "Creating"
run Update { maybeTemplateName = t } = putStrLn "Updating"

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
