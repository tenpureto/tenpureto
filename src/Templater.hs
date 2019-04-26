{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Templater where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Maybe
import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Logging
import           Path
import           Path.IO
import           System.IO                      ( hClose )
import qualified Data.Text.ICU                 as ICU
import           Data.Text.Encoding
import           Data.Text.ICU.Replace
import           Templater.CaseConversion
import           Git

data TemplaterSettings = TemplaterSettings
    { templaterFromVariables :: InsOrdHashMap Text Text
    , templaterToVariables :: InsOrdHashMap Text Text
    , templaterExcludes :: Set Text
    }

newtype CompiledTemplaterSettings = CompiledTemplaterSettings
    { translate :: Text -> Text
    }

data TemplaterException = TemplaterException
    deriving (Show, Exception)

expandReplacement :: (Text, Text) -> [(Text, Text)]
expandReplacement (a, b) =
    [ (templateValueText av, templateValueText bv)
    | av <- variations a
    , bv <- variations b
    , sameStyle av bv
    ]
  where
    variations text = Set.toList . Set.fromList $ concatMap
        styleVariations
        (textToTemplateValues text)

expandReplacements
    :: InsOrdHashMap Text Text -> InsOrdHashMap Text Text -> [(Text, Text)]
expandReplacements fv tv = concatMap expandReplacement
    $ InsOrdHashMap.elems (InsOrdHashMap.intersectionWith (,) fv tv)

compileSettings
    :: MonadLog m => TemplaterSettings -> m CompiledTemplaterSettings
compileSettings TemplaterSettings { templaterFromVariables = tfv, templaterToVariables = ttv, templaterExcludes = _ }
    = let replacements = expandReplacements tfv ttv
      in
          do
              logDebug $ "From variables:" <+> align
                  (sep
                      [ pretty f <+> "->" <+> pretty t
                      | (f, t) <- InsOrdHashMap.toList tfv
                      ]
                  )
              logDebug $ "To variables:" <+> align
                  (sep
                      [ pretty f <+> "->" <+> pretty t
                      | (f, t) <- InsOrdHashMap.toList ttv
                      ]
                  )
              logDebug
                  $   "Replacements:"
                  <+> align
                          (sep
                              [ pretty f <+> "->" <+> pretty t
                              | (f, t) <- replacements
                              ]
                          )
              return CompiledTemplaterSettings
                  { translate = replaceVariables replacements
                  }

replaceVariables :: [(Text, Text)] -> Text -> Text
replaceVariables []    = id
replaceVariables rules = replaceAll regex replace
  where
    rulesMap = Map.fromList rules
    quote text = "\\Q" <> T.replace "\\E" "\\\\E" text <> "\\E"
    regex = ICU.regex [] $ T.intercalate "|" $ fmap (quote . fst) rules
    findReplacement match = fromMaybe matchText (Map.lookup matchText rulesMap)
        where matchText = fold $ ICU.group 0 match
    replace = rtfn findReplacement

data FileContent = BinaryContent ByteString
                 | TextContent Text (Text -> ByteString)

mapContent :: (Text -> Text) -> FileContent -> FileContent
mapContent f (TextContent t enc) = TextContent (f t) enc
mapContent f (BinaryContent b  ) = BinaryContent b

detectEncoding :: ByteString -> FileContent
detectEncoding bs = case decodeUtf8' bs of
    Right t -> TextContent t encodeUtf8
    Left  _ -> BinaryContent bs

contentToByteString :: FileContent -> ByteString
contentToByteString (TextContent t enc) = enc t
contentToByteString (BinaryContent bs ) = bs

translateFile
    :: MonadThrow m
    => CompiledTemplaterSettings
    -> Path Rel File
    -> m (Path Rel File)
translateFile settings =
    parseRelFile . T.unpack . translate settings . T.pack . fromRelFile

copyAbsFile
    :: (MonadIO m, MonadLog m)
    => CompiledTemplaterSettings
    -> Path Abs File
    -> Path Abs File
    -> (ByteString -> m ())
    -> m ()
copyAbsFile settings src dst write = do
    logDebug $ "Copying" <+> pretty src <+> "to" <+> pretty dst
    byteContent <- liftIO $ BS.readFile (toFilePath src)
    let content           = detectEncoding byteContent
        translatedContent = mapContent (translate settings) content
        translated        = contentToByteString translatedContent
    ensureDir (parent dst)
    write translated

copyRelFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => CompiledTemplaterSettings
    -> Path Abs Dir
    -> Path Abs Dir
    -> Path Rel File
    -> m (Path Rel File)
copyRelFile settings src dst srcFile = do
    dstFile <- translateFile settings srcFile
    let srcAbsFile = src </> srcFile
        dstAbsFile = dst </> dstFile
    copyAbsFile settings
                srcAbsFile
                dstAbsFile
                (liftIO . BS.writeFile (toFilePath dstAbsFile))
    copyPermissions srcAbsFile dstAbsFile
    return dstFile

moveRelFile
    :: (MonadIO m, MonadMask m, MonadLog m)
    => CompiledTemplaterSettings
    -> Path Abs Dir
    -> Path Abs Dir
    -> Path Rel File
    -> m (Path Rel File, (Path Abs File, Path Rel File))
moveRelFile settings src dst srcFile = do
    dstFile <- translateFile settings srcFile
    let srcAbsFile = src </> srcFile
        dstAbsFile = dst </> dstFile
        dstFileDir = parent dstAbsFile
    ensureDir dstFileDir
    bracket
            (liftIO $ openBinaryTempFile dstFileDir
                                         (toFilePath (filename dstFile))
            )
            (liftIO . hClose . snd)
        $ \(tmpFile, tmpHandle) -> do
              copyAbsFile settings
                          srcAbsFile
                          tmpFile
                          (liftIO . BS.hPut tmpHandle)
              copyPermissions srcAbsFile tmpFile
              logDebug $ "Removing" <+> pretty srcAbsFile
              removeFile srcAbsFile
              return (srcFile, (tmpFile, dstFile))

copy
    :: (MonadIO m, MonadThrow m, MonadLog m, MonadGit m)
    => CompiledTemplaterSettings
    -> GitRepository
    -> Path Abs Dir
    -> m [Path Rel File]
copy settings repo dst =
    let src = repositoryPath repo
    in  do
            files <- listFiles repo
            traverse (copyRelFile settings src dst) files

move
    :: (MonadIO m, MonadMask m, MonadLog m, MonadGit m)
    => CompiledTemplaterSettings
    -> GitRepository
    -> m [Path Rel File]
move settings repo =
    let dir = repositoryPath repo
        renameFile' (a, b) = do
            logDebug $ "Moving" <+> pretty a <+> "to" <+> pretty (dir </> b)
            renameFile a (dir </> b)
    in  do
            files       <- listFiles repo
            moveResults <- traverse (moveRelFile settings dir dir) files
            let oldFiles = fmap fst moveResults
                actions  = fmap snd moveResults
            traverse_ renameFile' actions
            let newFiles = fmap snd actions
                changes  = Set.fromList oldFiles <> Set.fromList newFiles
            return $ Set.toList changes
