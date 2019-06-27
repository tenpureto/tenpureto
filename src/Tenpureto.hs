module Tenpureto
    ( module Tenpureto
    , TenpuretoException(..)
    )
where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Resource

import           Data.List
import           Data.Maybe
import           Data.Either
import           Data.Either.Combinators
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Foldable
import qualified Data.Text.ICU                 as ICU
import           Data.Functor
import           Text.Dot

import           Tenpureto.Data
import           Tenpureto.Messages
import           Tenpureto.Effects.Logging
import           Tenpureto.Effects.Terminal
import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Process
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.TemplateLoader
import           Tenpureto.MergeOptimizer
import           Tenpureto.Templater
import           Tenpureto.Internal

data RemoteChangeMode = PushDirectly
                      | UpstreamPullRequest { pullRequestSettings :: PullRequestSettings }

makeFinalTemplateConfiguration
    :: Member UI r
    => PreliminaryProjectConfiguration
    -> Sem r FinalTemplateConfiguration
makeFinalTemplateConfiguration = inputTemplateConfiguration

makeFinalProjectConfiguration
    :: Member UI r
    => TemplateInformation
    -> PreliminaryProjectConfiguration
    -> Sem r FinalProjectConfiguration
makeFinalProjectConfiguration = inputProjectConfiguration

makeFinalUpdateConfiguration
    :: Member UI r
    => PreliminaryProjectConfiguration
    -> Sem r FinalUpdateConfiguration
makeFinalUpdateConfiguration = inputUpdateConfiguration

buildTemplaterSettings
    :: TemplateYaml -> FinalProjectConfiguration -> TemplaterSettings
buildTemplaterSettings TemplateYaml { variables = templateValues, excludes = templateExcludes } FinalProjectConfiguration { variableValues = values }
    = TemplaterSettings
        { templaterFromVariables = templateValues
        , templaterToVariables   = (InsOrdHashMap.fromList . Map.toList) values
        , templaterExcludes      = templateExcludes
        }

createProject
    :: Members
           '[Git, UI, FileSystem, Terminal, Logging, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> Sem r ()
createProject projectConfiguration =
    withPreparedTemplate projectConfiguration
        $ \template finalTemplateConfiguration templaterSettings ->
              let dst = targetDirectory finalTemplateConfiguration
              in
                  do
                      project          <- initRepository dst
                      compiledSettings <- compileSettings templaterSettings
                      files            <- copy compiledSettings
                                               template
                                               (repositoryPath project)
                      addFiles project files
                      _ <- commit
                          project
                          (commitCreateMessage finalTemplateConfiguration)
                      sayLn $ projectCreated dst

updateProject
    :: Members
           '[Git, UI, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> Sem r ()
updateProject projectConfiguration = do
    finalUpdateConfiguration <- makeFinalUpdateConfiguration
        projectConfiguration
    logDebug
        $  "Final update configuration"
        <> line
        <> (indent 4 . pretty) finalUpdateConfiguration
    withPreparedTemplate projectConfiguration
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
                                          mergeResult <- mergeBranch project c
                                          case mergeResult of
                                              MergeSuccess -> do
                                                  _ <- commit
                                                      project
                                                      (commitUpdateMergeMessage
                                                          finalTemplateConfiguration
                                                      )
                                                  sayLn
                                                      $ projectUpdated
                                                            (repositoryPath
                                                                project
                                                            )
                                              MergeConflicts _ ->
                                                  sayLn
                                                      $ projectUpdatedWithConflicts
                                                            (repositoryPath
                                                                project
                                                            )
                                      Nothing ->
                                          sayLn noRelevantTemplateChanges

withPreparedTemplate
    :: Members
           '[Git, UI, Terminal, Logging, FileSystem, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> (  GitRepository
       -> FinalTemplateConfiguration
       -> TemplaterSettings
       -> Sem r ()
       )
    -> Sem r ()
withPreparedTemplate projectConfiguration block = do
    logDebug
        $  "Preliminary project configuration:"
        <> line
        <> (indent 4 . pretty) projectConfiguration
    finalTemplateConfiguration <- makeFinalTemplateConfiguration
        projectConfiguration
    withClonedRepository
            (buildRepositoryUrl $ selectedTemplate finalTemplateConfiguration)
        $ \repository -> do
              templateInformation <- loadTemplateInformation'
                  (selectedTemplate finalTemplateConfiguration)
                  repository
              logDebug
                  $  "Template information"
                  <> line
                  <> (indent 4 . pretty) templateInformation
              finalProjectConfiguration <- makeFinalProjectConfiguration
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
    :: Members '[Git, Logging] r
    => Path Abs Dir
    -> Sem r PreliminaryProjectConfiguration
loadExistingProjectConfiguration projectPath =
    withRepository projectPath $ \project -> do
        logInfo
            $   "Loading template configuration from"
            <+> pretty templateYamlFile
        templateYamlContent <- getWorkingCopyFile project templateYamlFile
        previousCommit <- findCommitByMessage project commitMessagePattern
        previousCommitMessage <- traverse (getCommitMessage project)
                                          previousCommit
        let yaml = templateYamlContent >>= (rightToMaybe . parseTemplateYaml)
        return PreliminaryProjectConfiguration
            { preSelectedTemplate       = extractTemplateName
                                              =<< previousCommitMessage
            , preTargetDirectory        = Just projectPath
            , prePreviousTemplateCommit = previousCommit
            , preSelectedBranches       = fmap features yaml
            , preVariableValues         = fmap
                                              ( Map.fromList
                                              . InsOrdHashMap.toList
                                              . variables
                                              )
                                              yaml
            }

prepareTemplate
    :: Members '[Git, UI, Terminal, Logging, Error TenpuretoException] r
    => GitRepository
    -> TemplateInformation
    -> FinalProjectConfiguration
    -> Sem r TemplateYaml
prepareTemplate repository template configuration =
    let
        resolve _          []        = return ()
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
            mergeResult <- mergeBranch repository ("origin/" <> branchName b)
            case mergeResult of
                MergeSuccess             -> return ()
                MergeConflicts conflicts -> resolve d conflicts
            _ <- commit repository ("Merge " <> branchName b)
            return d
        branchesToMerge =
            reorderBranches $ includeMergeBranches template $ projectBranches
                configuration
    in
        case branchesToMerge of
            []    -> throw TenpuretoEmptySelection
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
    , excludes  = excludes descriptor
    }

replaceInFunctor :: (Functor f, Eq a) => a -> a -> f a -> f a
replaceInFunctor from to = fmap (\v -> if from == v then to else v)

replaceVariableInYaml :: Text -> Text -> TemplateYaml -> TemplateYaml
replaceVariableInYaml old new descriptor = TemplateYaml
    { variables = replaceInFunctor old new (variables descriptor)
    , features  = features descriptor
    , excludes  = excludes descriptor
    }

commit_
    :: Members '[Git, Error TenpuretoException] r
    => GitRepository
    -> Text
    -> Sem r Committish
commit_ repo message =
    commit repo message >>= maybe (throw TenpuretoEmptyChangeset) return

generateTemplateGraph
    :: Members
           '[Git, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Sem r ()
generateTemplateGraph template =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        templateInformation <- loadTemplateInformation' template repo
        let nodeAttributes branch =
                [("label", T.unpack (branchName branch)), ("shape", "box")]
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
        sayLn $ pretty (showDot graph)

listTemplateBranches
    :: Members
           '[Git, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> [BranchFilter]
    -> Sem r ()
listTemplateBranches template branchFilters =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        templateInformation <- loadTemplateInformation' template repo
        let branches = getTemplateBranches branchFilters templateInformation
        traverse_ (sayLn . pretty . branchName) branches

renameTemplateBranch
    :: Members
           '[Git, GitServer, UI, Terminal, FileSystem, Process, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Text
    -> Text
    -> Bool
    -> Sem r ()
renameTemplateBranch template oldBranch newBranch interactive =
    runTemplateChange template interactive PushDirectly $ \repo -> do
        templateInformation <- loadTemplateInformation' template repo
        mainBranch          <- getTemplateBranch templateInformation oldBranch
        let bis = branchesInformation templateInformation
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
        let deleteOld = DeleteBranch { destinationRef = BranchRef oldBranch }
        pushNew <-
            renameOnBranch mainBranch
                <&> \c -> CreateBranch { sourceCommit   = c
                                       , destinationRef = BranchRef newBranch
                                       }
        pushChildren <- traverse
            (\bi -> renameOnBranch bi <&> \c -> UpdateBranch
                { sourceCommit     = c
                , destinationRef   = BranchRef $ branchName bi
                , pullRequestRef   = BranchRef $ branchName bi
                , pullRequestTitle = pullRequestRenameBranchTitle
                                         (branchName bi)
                                         oldBranch
                                         newBranch
                }
            )
            childBranches
        return $ deleteOld : pushNew : pushChildren

propagateTemplateBranchChanges
    :: Members
           '[Git, GitServer, UI, Terminal, FileSystem, Process, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Text
    -> RemoteChangeMode
    -> Sem r ()
propagateTemplateBranchChanges template sourceBranch pushMode =
    runTemplateChange template False pushMode $ \repo -> do
        templateInformation <- loadTemplateInformation' template repo
        branch <- getTemplateBranch templateInformation sourceBranch
        let childBranches = getTemplateBranches
                [BranchFilterChildOf sourceBranch]
                templateInformation
        let parentBranches = getTemplateBranches
                [BranchFilterParentOf sourceBranch]
                templateInformation
        let prBranch bi = branchName branch <> "/" <> branchName bi
        let keepNeedsMerge src dst =
                gitDiffHasCommits repo (branchCommit src) (branchCommit dst)
                    <&> \needs -> if needs then Just (src, dst) else Nothing
        let
            pushspec (src, dst) = UpdateBranch
                { sourceCommit     = branchCommit src
                , destinationRef   = BranchRef $ branchName dst
                , pullRequestRef   = BranchRef $ prBranch dst
                , pullRequestTitle = pullRequestBranchIntoBranchTitle
                                         (branchName src)
                                         (branchName dst)
                }
        mergesToChildren <-
            catMaybes <$> traverse (keepNeedsMerge branch) childBranches
        mergesFromParents <-
            catMaybes <$> traverse (flip keepNeedsMerge branch) parentBranches
        return $ fmap pushspec (mergesFromParents <> mergesToChildren)

changeTemplateVariableValue
    :: Members
           '[Git, GitServer, UI, Terminal, FileSystem, Process, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Text
    -> Text
    -> Bool
    -> Sem r ()
changeTemplateVariableValue template oldValue newValue interactive =
    runTemplateChange template interactive PushDirectly $ \repo -> do
        bis <- branchesInformation <$> loadTemplateInformation' template repo
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
                return UpdateBranch
                    { sourceCommit     = c
                    , destinationRef   = BranchRef $ branchName bi
                    , pullRequestRef   = BranchRef $ branchName bi
                    , pullRequestTitle = pullRequestChangeVariableTitle
                                             (branchName bi)
                                             oldValue
                                             newValue
                    }
        traverse changeOnBranch branches

runTemplateChange
    :: Members
           '[Git, GitServer, UI, Terminal, Process, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Bool
    -> RemoteChangeMode
    -> (GitRepository -> Sem r [PushSpec])
    -> Sem r ()
runTemplateChange template interactive changeMode f =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        let confirmUpdate src (BranchRef ref) = do
                msg <- changesForBranchMessage ref <$> gitLogDiff
                    repo
                    src
                    (Committish $ "remotes/origin/" <> ref)
                sayLn msg
                if not interactive
                    then return src
                    else do
                        shouldRunShell <- confirmShellToAmend
                        if shouldRunShell
                            then do
                                runShell (repositoryPath repo)
                                getCurrentHead repo
                            else return src
            confirmCommit refspec@(DeleteBranch _) = return refspec
            confirmCommit CreateBranch { sourceCommit = src, destinationRef = dst }
                = confirmUpdate src dst <&> \c ->
                    CreateBranch { sourceCommit = c, destinationRef = dst }

            confirmCommit UpdateBranch { sourceCommit = src, destinationRef = dst, pullRequestRef = prRef, pullRequestTitle = title }
                = confirmUpdate src dst <&> \c -> UpdateBranch
                    { sourceCommit     = c
                    , destinationRef   = dst
                    , pullRequestRef   = prRef
                    , pullRequestTitle = title
                    }
        let pullRequest _ (DeleteBranch (BranchRef dst)) = do
                sayLn $ deleteBranchManually dst
                return $ Left $ TenpuretoBranchNotDeleted dst
            pullRequest _ (CreateBranch _ (BranchRef dst)) = do
                sayLn $ createBranchManually dst
                return $ Left $ TenpuretoBranchNotCreated dst
            pullRequest settings UpdateBranch { sourceCommit = c, destinationRef = BranchRef dst, pullRequestRef = BranchRef src, pullRequestTitle = title }
                = runError $ createOrUpdatePullRequest repo
                                                       settings
                                                       c
                                                       title
                                                       (internalBranchPrefix <> src)
                                                       dst
        let pushRefsToServer PushDirectly changes = pushRefs repo changes
            pushRefsToServer UpstreamPullRequest { pullRequestSettings = settings } changes
                = do
                    errors <- lefts <$> traverse (pullRequest settings) changes
                    case errors of
                        [] -> return ()
                        e  -> throw $ MultipleExceptions e
        changes <- f repo >>= traverse confirmCommit
        if null changes
            then sayLn noRelevantTemplateChanges
            else do
                let collectPushRefs DeleteBranch { destinationRef = r } (d, c, u)
                        = (r : d, c, u)
                    collectPushRefs CreateBranch { destinationRef = r } (d, c, u)
                        = (d, r : c, u)
                    collectPushRefs UpdateBranch { destinationRef = r } (d, c, u)
                        = (d, c, r : u)
                    (deletes, creates, updates) =
                        foldr collectPushRefs ([], [], []) changes
                shouldPush <- confirmPush deletes creates updates
                if shouldPush
                    then pushRefsToServer changeMode changes
                    else throw CancelledException

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
    :: Member (Error TenpuretoException) r
    => TemplateInformation
    -> Text
    -> Sem r TemplateBranchInformation
getTemplateBranch templateInformation branch = maybe
    (throw $ TemplateBranchNotFoundException branch)
    return
    (findTemplateBranch templateInformation branch)
