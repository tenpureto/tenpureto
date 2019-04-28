{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tenpureto where

import           Data.List
import           Data.Maybe
import           Data.Either.Combinators
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Yaml                     as Y
import           Data
import qualified Data.Text.ICU                 as ICU
import           Data.Functor
import           Git
import           Console
import           UI
import           Logging
import           Templater
import           Path
import           Path.IO
import           System.Process.Typed
import           Tenpureto.Messages

data TenpuretoException = InvalidTemplateException Text Text
                        | InvalidTemplateBranchException Text Text
                        | CancelledException
                        | TenpuretoException Text
                        deriving (Exception)

instance Show TenpuretoException where
    show (InvalidTemplateException template reason) =
        T.unpack $ "Template \"" <> template <> "\" is not valid: " <> reason
    show (InvalidTemplateBranchException branch reason) =
        T.unpack $ "Branch \"" <> branch <> "\" is not valid: " <> reason
    show CancelledException           = "Cancelled"
    show (TenpuretoException message) = T.unpack message

templateYamlFile :: Path Rel File
templateYamlFile = [relfile|.template.yaml|]

makeFinalTemplateConfiguration
    :: (MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalTemplateConfiguration
makeFinalTemplateConfiguration True  = unattendedTemplateConfiguration
makeFinalTemplateConfiguration False = inputTemplateConfiguration

makeFinalProjectConfiguration
    :: (MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> TemplateInformation
    -> PreliminaryProjectConfiguration
    -> m FinalProjectConfiguration
makeFinalProjectConfiguration True  = unattendedProjectConfiguration
makeFinalProjectConfiguration False = inputProjectConfiguration

makeFinalUpdateConfiguration
    :: (MonadMask m, MonadIO m, MonadConsole m)
    => Bool
    -> PreliminaryProjectConfiguration
    -> m FinalUpdateConfiguration
makeFinalUpdateConfiguration True  = unattendedUpdateConfiguration
makeFinalUpdateConfiguration False = inputUpdateConfiguration

buildTemplaterSettings
    :: TemplateYaml -> FinalProjectConfiguration -> TemplaterSettings
buildTemplaterSettings TemplateYaml { variables = templateValues } FinalProjectConfiguration { variableValues = values }
    = TemplaterSettings
        { templaterFromVariables = templateValues
        , templaterToVariables   = (InsOrdHashMap.fromList . Map.toList) values
        , templaterExcludes      = Set.empty
        }

createProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
createProject projectConfiguration unattended =
    withPreparedTemplate projectConfiguration unattended
        $ \template finalTemplateConfiguration templaterSettings ->
              let dst = targetDirectory finalTemplateConfiguration
              in
                  do
                      createDir dst
                      project          <- initRepository dst
                      compiledSettings <- compileSettings templaterSettings
                      files            <- copy compiledSettings
                                               template
                                               (repositoryPath project)
                      addFiles project files
                      commit
                          project
                          (commitCreateMessage finalTemplateConfiguration)
                      sayLn $ projectCreated dst

updateProject
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> m ()
updateProject projectConfiguration unattended = do
    finalUpdateConfiguration <- makeFinalUpdateConfiguration
        unattended
        projectConfiguration
    logDebug
        $  "Final update configuration"
        <> line
        <> (indent 4 . pretty) finalUpdateConfiguration
    withPreparedTemplate projectConfiguration unattended
        $ \template finalTemplateConfiguration templaterSettings ->
              withRepository (targetDirectory finalTemplateConfiguration)
                  $ \project ->
                        withNewWorktree
                                project
                                (previousTemplateCommit finalUpdateConfiguration
                                )
                            $ \staging -> do
                                  compiledSettings <- compileSettings
                                      templaterSettings
                                  files <- copy compiledSettings
                                                template
                                                (repositoryPath staging)
                                  addFiles staging files
                                  result <- commit
                                      staging
                                      (commitUpdateMessage
                                          finalTemplateConfiguration
                                      )
                                  case result of
                                      Just (Committish c) -> do
                                          logInfo
                                              $   "Updated template commit:"
                                              <+> pretty c
                                          mergeBranch project
                                                      c
                                                      (const (return ()))
                                          commit
                                              project
                                              (commitUpdateMergeMessage
                                                  finalTemplateConfiguration
                                              )
                                          sayLn $ projectUpdated
                                              (repositoryPath project)
                                      Nothing ->
                                          sayLn noRelevantTemplateChanges

withPreparedTemplate
    :: (MonadIO m, MonadMask m, MonadGit m, MonadConsole m, MonadLog m)
    => PreliminaryProjectConfiguration
    -> Bool
    -> (  GitRepository
       -> FinalTemplateConfiguration
       -> TemplaterSettings
       -> m ()
       )
    -> m ()
withPreparedTemplate projectConfiguration unattended block = do
    logDebug
        $  "Preliminary project configuration:"
        <> line
        <> (indent 4 . pretty) projectConfiguration
    finalTemplateConfiguration <- makeFinalTemplateConfiguration
        unattended
        projectConfiguration
    withClonedRepository
            (buildRepositoryUrl $ selectedTemplate finalTemplateConfiguration)
        $ \repository -> do
              templateInformation <- loadTemplateInformation
                  (selectedTemplate finalTemplateConfiguration)
                  repository
              logDebug
                  $  "Template information"
                  <> line
                  <> (indent 4 . pretty) templateInformation
              finalProjectConfiguration <- makeFinalProjectConfiguration
                  unattended
                  templateInformation
                  projectConfiguration
              logDebug
                  $  "Final project configuration"
                  <> line
                  <> (indent 4 . pretty) finalProjectConfiguration
              mergedTemplateYaml <- prepareTemplate
                  repository
                  templateInformation
                  finalProjectConfiguration
              logDebug
                  $  "Merged template YAML"
                  <> line
                  <> (indent 4 . pretty) mergedTemplateYaml
              let templaterSettings = buildTemplaterSettings
                      mergedTemplateYaml
                      finalProjectConfiguration
              block repository finalTemplateConfiguration templaterSettings

parseTemplateYaml :: ByteString -> Maybe TemplateYaml
parseTemplateYaml yaml =
    let info :: Either Y.ParseException TemplateYaml
        info = Y.decodeEither' (BS.toStrict yaml)
    in  rightToMaybe info

loadExistingProjectConfiguration
    :: (MonadGit m, MonadLog m)
    => Path Abs Dir
    -> m PreliminaryProjectConfiguration
loadExistingProjectConfiguration projectPath =
    withRepository projectPath $ \project -> do
        logInfo
            $   "Loading template configuration from"
            <+> pretty templateYamlFile
        templateYamlContent    <- getWorkingCopyFile project templateYamlFile
        previousTemplateCommit <- findCommit project commitMessagePattern
        previousCommitMessage  <- traverse (getCommitMessage project)
                                           previousTemplateCommit
        let yaml = templateYamlContent >>= parseTemplateYaml
        return PreliminaryProjectConfiguration
            { preSelectedTemplate       = extractTemplateName
                                              =<< previousCommitMessage
            , preTargetDirectory        = Just projectPath
            , prePreviousTemplateCommit = previousTemplateCommit
            , preSelectedBranches       = fmap features yaml
            , preVariableValues         = fmap
                                              ( Map.fromList
                                              . InsOrdHashMap.toList
                                              . variables
                                              )
                                              yaml
            }

loadBranchConfiguration
    :: (MonadThrow m, MonadGit m)
    => GitRepository
    -> Text
    -> m (Maybe TemplateBranchInformation)
loadBranchConfiguration repo branch = runMaybeT $ do
    descriptor <- MaybeT $ getBranchFile
        repo
        (T.pack "remotes/origin/" <> branch)
        templateYamlFile
    info <- MaybeT $ return $ parseTemplateYaml descriptor
    let checkFeatures f = if Set.member branch f then Just f else Nothing
    fb <- MaybeT $ return $ checkFeatures (features info)
    return $ TemplateBranchInformation
        { branchName       = branch
        , isBaseBranch     = fb == Set.singleton branch
        , requiredBranches = fb
        , branchVariables  = variables info
        , templateYaml     = info
        }

hasBaseBranches :: [TemplateBranchInformation] -> Bool
hasBaseBranches = any isBaseBranch

loadTemplateInformation
    :: (MonadThrow m, MonadGit m)
    => Text
    -> GitRepository
    -> m TemplateInformation
loadTemplateInformation repositoryName repository = do
    branches             <- listBranches repository
    branchConfigurations <- traverse (loadBranchConfiguration repository)
        $ sort branches
    let bi = catMaybes branchConfigurations
    _ <- if hasBaseBranches bi
        then return ()
        else throwM $ InvalidTemplateException repositoryName
                                               "no base branches found"
    return $ TemplateInformation { branchesInformation = bi }

prepareTemplate
    :: (MonadIO m, MonadGit m, MonadConsole m)
    => GitRepository
    -> TemplateInformation
    -> FinalProjectConfiguration
    -> m TemplateYaml
prepareTemplate repository template configuration =
    let
        branch = "template"
        resolve descriptor []        = return ()
        resolve descriptor conflicts = if templateYamlFile `elem` conflicts
            then
                writeAddFile repository
                             templateYamlFile
                             ((BS.fromStrict . Y.encode) descriptor)
                    >> resolve descriptor (delete templateYamlFile conflicts)
            else
                inputResolutionStrategy (repositoryPath repository) conflicts
                    >>= \case
                            AlreadyResolved -> return ()
                            MergeTool ->
                                runMergeTool repository >> sayLn mergeSuccess
        mergeTemplateBranch a b =
            let bi = find ((==) b . branchName) (branchesInformation template)
                d  = maybe a ((<>) a . templateYaml) bi
            in  do
                    mergeBranch repository ("origin/" <> b) (resolve d)
                    commit repository ("Merge " <> b)
                    return d
    in
        do
            checkoutBranch repository (baseBranch configuration) (Just branch)
            foldlM mergeTemplateBranch mempty (featureBranches configuration)

replaceInSet :: Ord a => a -> a -> Set a -> Set a
replaceInSet from to = Set.insert to . Set.delete from

replaceBranchInYaml :: Text -> Text -> TemplateYaml -> TemplateYaml
replaceBranchInYaml old new descriptor = TemplateYaml
    { variables = variables descriptor
    , features  = replaceInSet old new (features descriptor)
    }

replaceInFunctor :: (Functor f, Eq a) => a -> a -> f a -> f a
replaceInFunctor from to = fmap (\v -> if from == v then to else v)

replaceVariableInYaml :: Text -> Text -> TemplateYaml -> TemplateYaml
replaceVariableInYaml old new descriptor = TemplateYaml
    { variables = replaceInFunctor old new (variables descriptor)
    , features  = features descriptor
    }

commit_ :: (MonadThrow m, MonadGit m) => GitRepository -> Text -> m Committish
commit_ repo message =
    commit repo message
        >>= maybe
                (throwM $ TenpuretoException
                    "Failed to create a rename commit: empty changeset"
                )
                return

renameTemplateBranch
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> Text
    -> Text
    -> Bool
    -> m ()
renameTemplateBranch template oldBranch newBranch interactive =
    runTemplateChange template interactive $ \repo -> do
        bis <- branchesInformation <$> loadTemplateInformation template repo
        let throwOldBranchNotFound = throwM $ InvalidTemplateBranchException
                oldBranch
                "branch not found"
            refspec b c = Refspec (Just c) (branchRef b)
            renameOnBranch bi = do
                checkoutBranch repo (branchName bi) Nothing
                let descriptor = templateYaml bi
                    newDescriptor =
                        replaceBranchInYaml oldBranch newBranch descriptor
                writeAddFile repo
                             templateYamlFile
                             ((BS.fromStrict . Y.encode) newDescriptor)
                commit_ repo (commitRenameBranchMessage oldBranch newBranch)
            maybeMainBranch = find (isBranch oldBranch) bis
            childBranches   = filter (isChildBranch oldBranch) bis
        mainBranch <- maybe throwOldBranchNotFound return maybeMainBranch
        let deleteOld = Refspec Nothing (branchRef oldBranch)
        pushNew      <- refspec newBranch <$> renameOnBranch mainBranch
        pushChildren <- traverse
            (\bi -> refspec (branchName bi) <$> renameOnBranch bi)
            childBranches
        return $ deleteOld : pushNew : pushChildren


changeTemplateVariableValue
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> Text
    -> Text
    -> Bool
    -> m ()
changeTemplateVariableValue template oldValue newValue interactive =
    runTemplateChange template interactive $ \repo -> do
        bis <- branchesInformation <$> loadTemplateInformation template repo
        templaterSettings <- compileSettings $ TemplaterSettings
            { templaterFromVariables = InsOrdHashMap.singleton "Variable"
                                                               oldValue
            , templaterToVariables = InsOrdHashMap.singleton "Variable" newValue
            , templaterExcludes = Set.empty
            }
        let branches = filter (hasVariableValue oldValue) bis
            changeOnBranch bi = do
                checkoutBranch repo (branchName bi) Nothing
                files <- move templaterSettings repo
                addFiles repo files
                let descriptor = templateYaml bi
                    newDescriptor =
                        replaceVariableInYaml oldValue newValue descriptor
                writeAddFile repo
                             templateYamlFile
                             ((BS.fromStrict . Y.encode) newDescriptor)
                c <- commit_ repo
                             (commitChangeVariableMessage oldValue newValue)
                return $ Refspec (Just c) (branchRef (branchName bi))
        traverse changeOnBranch branches

runTemplateChange
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> Bool
    -> (GitRepository -> m [Refspec])
    -> m ()
runTemplateChange template interactive f =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        let confirmCommit (Refspec Nothing _) = return Nothing
            confirmCommit refspec@(Refspec (Just c) (Ref _ ref)) = do
                msg <- commitOnBranchMessage ref <$> getCommitContent repo c
                sayLn msg
                if not interactive
                    then return $ Just refspec
                    else do
                        shouldRunShell <- confirm confirmShellToAmendMessage
                        if shouldRunShell
                            then do
                                runShell (repositoryPath repo)
                                newCommit <- getCurrentHead repo
                                return $ Just
                                    (Refspec
                                        { sourceRef      = Just newCommit
                                        , destinationRef = destinationRef
                                                               refspec
                                        }
                                    )
                            else return $ Just refspec
        changes <- catMaybes <$> (f repo >>= traverse confirmCommit)
        let (deletes, updates) = partition (isNothing . sourceRef) changes
        shouldPush <- confirm $ confirmPushMessage deletes updates
        if shouldPush then pushRefs repo changes else throwM CancelledException

runShell :: MonadIO m => Path Abs Dir -> m ()
runShell dir = runProcess_ $ setWorkingDir (toFilePath dir) $ shell "$SHELL"

commitMessagePattern :: Text
commitMessagePattern = "^Template: .*$"

extractTemplateNameRegex :: ICU.Regex
extractTemplateNameRegex = ICU.regex [ICU.Multiline] "^Template: (.*)$"

extractTemplateName :: Text -> Maybe Text
extractTemplateName msg = ICU.find extractTemplateNameRegex msg >>= ICU.group 1

orgRepoRegex :: ICU.Regex
orgRepoRegex = ICU.regex [] "^[\\w-]+/[\\w.-]+$"

buildRepositoryUrl :: Text -> RepositoryUrl
buildRepositoryUrl url
    | isJust (ICU.find orgRepoRegex url)
    = RepositoryUrl $ "git@github.com:" <> url <> ".git"
    | otherwise
    = RepositoryUrl url

isBranch :: Text -> TemplateBranchInformation -> Bool
isBranch branch bi = branch == branchName bi

isChildBranch :: Text -> TemplateBranchInformation -> Bool
isChildBranch branch bi =
    branch /= branchName bi && Set.member branch (requiredBranches bi)

hasVariableValue :: Text -> TemplateBranchInformation -> Bool
hasVariableValue value bi =
    value `elem` InsOrdHashMap.elems (branchVariables bi)
