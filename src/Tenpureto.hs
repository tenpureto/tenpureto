{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

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
                      project <- initRepository dst
                      files   <- copy templaterSettings
                                      template
                                      (repositoryPath project)
                      addFiles project files
                      commit
                          project
                          (commitCreateMessage finalTemplateConfiguration)
                      sayLn $ "Created " <> pretty dst

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
                                  files <- copy templaterSettings
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
                                          sayLn
                                              $  "Updated "
                                              <> (pretty . repositoryPath)
                                                     project
                                      Nothing ->
                                          sayLn
                                              "There are no relevant changes in the template."

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
                            MergeTool       -> runMergeTool repository
                                >> sayLn "Successfully merged."
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

renameTemplateBranch
    :: (MonadMask m, MonadGit m, MonadLog m, MonadConsole m)
    => Text
    -> Text
    -> Text
    -> m ()
renameTemplateBranch template oldBranch newBranch =
    withClonedRepository (buildRepositoryUrl template) $ \repo -> do
        templateInformation <- loadTemplateInformation template repo
        let bis = branchesInformation templateInformation
            throwEmptyChangeset =
                throwM $ TenpuretoException
                    "Failed to create a rename commit: empty changeset"
            throwOldBranchNotFound = throwM $ InvalidTemplateBranchException
                oldBranch
                "branch not found"
            renameOnBranch bi = do
                checkoutBranch repo (branchName bi) Nothing
                let descriptor = templateYaml bi
                    newDescriptor =
                        replaceBranchInYaml oldBranch newBranch descriptor
                writeAddFile repo
                             templateYamlFile
                             ((BS.fromStrict . Y.encode) newDescriptor)
                maybeRenameCommit <- commit
                    repo
                    (commitRenameBranchMessage oldBranch newBranch)
                maybe throwEmptyChangeset return maybeRenameCommit
            maybeMainBranch = find ((==) oldBranch . branchName) bis
            relatedBranches =
                ( filter (Set.member oldBranch . requiredBranches)
                    . filter ((/=) oldBranch . branchName)
                    )
                    bis
            getCommitMessage (commit, branch) = do
                content <- getCommitContent repo commit
                return
                    $  "Commit on "
                    <> pretty branch
                    <> ":"
                    <> line
                    <> (indent 4 . pretty) content
        mainBranch <- maybe throwOldBranchNotFound return maybeMainBranch
        mainRenameCommit <- (, newBranch) <$> renameOnBranch mainBranch
        relatedRenameCommits <- traverse
            (\bi -> (, branchName bi) <$> renameOnBranch bi)
            relatedBranches
        let allCommits = mainRenameCommit : relatedRenameCommits
        commitMessages <- traverse getCommitMessage allCommits
        sayLn $ vsep commitMessages
        shouldPush <- confirm
            (  "Do you want to delete \""
            <> pretty oldBranch
            <> "\" and push "
            <> (if null relatedRenameCommits
                   then "this commit"
                   else "these commits"
               )
            <> " to "
            <> (fillSep . punctuate comma . fmap (dquotes . pretty . snd))
                   allCommits
            )
        if shouldPush
            then pushRefs
                repo
                ((Nothing, oldBranch) : fmap (\(c, b) -> (Just c, b)) allCommits
                )
            else throwM CancelledException

commitCreateMessage :: FinalTemplateConfiguration -> Text
commitCreateMessage cfg =
    "Create from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMessage :: FinalTemplateConfiguration -> Text
commitUpdateMessage cfg =
    "Update from a template\n\nTemplate: " <> selectedTemplate cfg

commitUpdateMergeMessage :: FinalTemplateConfiguration -> Text
commitUpdateMergeMessage cfg = "Merge " <> selectedTemplate cfg

commitRenameBranchMessage :: Text -> Text -> Text
commitRenameBranchMessage from to = "Rename " <> from <> " to " <> to

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
