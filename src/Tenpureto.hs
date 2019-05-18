{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tenpureto where

import           Data.List
import           Data.Maybe
import           Data.Either.Combinators
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
import qualified Data.Text.ICU                 as ICU
import           Data.Functor
import           Path
import           Path.IO
import           System.Process.Typed
import           Text.Dot

import           Data
import           Git
import           Console
import           UI
import           Logging
import           Templater
import           Tenpureto.Messages
import           Tenpureto.TemplateLoader
import           Tenpureto.MergeOptimizer

data TenpuretoException = InvalidTemplateBranchException Text Text
                        | CancelledException
                        | TenpuretoException Text
                        deriving (Exception)

instance Show TenpuretoException where
    show (InvalidTemplateBranchException branch reason) =
        T.unpack $ "Branch \"" <> branch <> "\" is not valid: " <> reason
    show CancelledException           = "Cancelled"
    show (TenpuretoException message) = T.unpack message

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
        previousTemplateCommit <- findCommitByMessage project commitMessagePattern
        previousCommitMessage  <- traverse (getCommitMessage project)
                                           previousTemplateCommit
        let yaml = templateYamlContent >>= (rightToMaybe . parseTemplateYaml)
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

prepareTemplate
    :: (MonadIO m, MonadThrow m, MonadGit m, MonadConsole m, MonadLog m)
    => GitRepository
    -> TemplateInformation
    -> FinalProjectConfiguration
    -> m TemplateYaml
prepareTemplate repository template configuration =
    let
        resolve descriptor []        = return ()
        resolve descriptor conflicts = if templateYamlFile `elem` conflicts
            then
                writeAddFile repository
                             templateYamlFile
                             (formatTemplateYaml descriptor)
                    >> resolve descriptor (delete templateYamlFile conflicts)
            else
                inputResolutionStrategy (repositoryPath repository) conflicts
                    >>= \case
                            AlreadyResolved -> return ()
                            MergeTool ->
                                runMergeTool repository >> sayLn mergeSuccess
        checkoutTemplateBranch a = do
            checkoutBranch repository (branchName a) Nothing
            return $ templateYaml a
        mergeTemplateBranch a b = do
            let d = ((<>) a . templateYaml) b
            mergeBranch repository ("origin/" <> branchName b) (resolve d)
            commit repository ("Merge " <> branchName b)
            return d
        branchesToMerge =
            reorderBranches $ includeMergeBranches template $ projectBranches
                configuration
    in
        case branchesToMerge of
            [] ->
                throwM $ TenpuretoException
                    "Cannot create a project from an empty selection"
            h : t -> do
                logInfo $ "Merging branches:" <> line <> (indent 4 . pretty)
                    (fmap branchName branchesToMerge)
                checkoutTemplateBranch h >>= flip (foldlM mergeTemplateBranch) t

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

generateTemplateGraph
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> m ()
generateTemplateGraph template =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        templateInformation <- loadTemplateInformation template repo
        let nodeAttributes branch =
                [("label", T.unpack (branchName branch)), ("shape", "box")]
        let nodeParents branch =
                Set.toList
                    . Set.delete (branchName branch)
                    . requiredBranches
                    $ branch
        let relevantBranches = filter
                (\b -> isBaseBranch b || isFeatureBranch b)
                (branchesInformation templateInformation)
        let nodes = map branchName relevantBranches `zip` relevantBranches
        let
            graph =
                netlistGraph
                        nodeAttributes
                        (Set.toList . getBranchParents templateInformation)
                        nodes
                    *> attribute ("layout", "dot")
                    *> attribute ("rankdir", "LR")
        liftIO $ putStrLn (showDot graph)

listTemplateBranches
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> [BranchFilter]
    -> m ()
listTemplateBranches template branchFilters =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        templateInformation <- loadTemplateInformation template repo
        let branches = getTemplateBranches branchFilters templateInformation
        liftIO $ traverse_ (putStrLn . T.unpack . branchName) branches

renameTemplateBranch
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> Text
    -> Text
    -> Bool
    -> m ()
renameTemplateBranch template oldBranch newBranch interactive =
    runTemplateChange template interactive $ \repo -> do
        templateInformation <- loadTemplateInformation template repo
        mainBranch          <- getTemplateBranch templateInformation oldBranch
        let bis = branchesInformation templateInformation
        let refspec b c = Refspec (Just c) (branchRef b)
        let
            renameOnBranch bi = do
                checkoutBranch repo (branchName bi) Nothing
                let descriptor = templateYaml bi
                    newDescriptor =
                        replaceBranchInYaml oldBranch newBranch descriptor
                writeAddFile repo
                             templateYamlFile
                             (formatTemplateYaml newDescriptor)
                commit_ repo (commitRenameBranchMessage oldBranch newBranch)
        let childBranches = filter (isChildBranch oldBranch) bis
        let deleteOld     = Refspec Nothing (branchRef oldBranch)
        pushNew      <- refspec newBranch <$> renameOnBranch mainBranch
        pushChildren <- traverse
            (\bi -> refspec (branchName bi) <$> renameOnBranch bi)
            childBranches
        return $ deleteOld : pushNew : pushChildren

propagateTemplateBranchChanges
    :: (MonadIO m, MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> Text
    -> Bool
    -> m ()
propagateTemplateBranchChanges template sourceBranch _ =
    runTemplateChange template False $ \repo -> do
        templateInformation <- loadTemplateInformation template repo
        branch <- getTemplateBranch templateInformation sourceBranch
        let childBranches = getTemplateBranches
                [BranchFilterChildOf sourceBranch]
                templateInformation
        let refspec bi =
                Refspec (Just $ branchCommit branch) (branchRef $ branchName bi)
        return $ map refspec childBranches

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
                             (formatTemplateYaml newDescriptor)
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
        let confirmCommit refspec@(Refspec Nothing _) = return refspec
            confirmCommit refspec@(Refspec (Just c) (Ref _ ref)) = do
                msg <- commitOnBranchMessage ref <$> getCommitContent repo c
                sayLn msg
                if not interactive
                    then return refspec
                    else do
                        shouldRunShell <- confirm confirmShellToAmendMessage
                                                  (Just False)
                        if shouldRunShell
                            then do
                                runShell (repositoryPath repo)
                                newCommit <- getCurrentHead repo
                                return $ Refspec
                                    { sourceRef      = Just newCommit
                                    , destinationRef = destinationRef refspec
                                    }
                            else return refspec
        changes <- f repo >>= traverse confirmCommit
        if null changes
            then sayLn noRelevantTemplateChanges
            else do
                let (deletes, updates) =
                        partition (isNothing . sourceRef) changes
                shouldPush <- confirm (confirmPushMessage deletes updates)
                                      (Just False)
                if shouldPush
                    then pushRefs repo changes
                    else throwM CancelledException

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

isChildBranch :: Text -> TemplateBranchInformation -> Bool
isChildBranch branch bi =
    branch /= branchName bi && Set.member branch (requiredBranches bi)

hasVariableValue :: Text -> TemplateBranchInformation -> Bool
hasVariableValue value bi =
    value `elem` InsOrdHashMap.elems (branchVariables bi)

getTemplateBranch
    :: MonadThrow m
    => TemplateInformation
    -> Text
    -> m TemplateBranchInformation
getTemplateBranch templateInformation branch = maybe
    (throwM $ InvalidTemplateBranchException branch "branch not found")
    return
    (findTemplateBranch templateInformation branch)
