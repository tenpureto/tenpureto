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
import           Data.Maybe
import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Logging
import           Path
import           Path.IO
import qualified Data.Text.ICU                 as ICU
import           Data.Text.Encoding
import           Data.Text.ICU.Replace
import           Templater.CaseConversion

data TemplaterSettings = TemplaterSettings
    { templaterVariables :: [(Text, Text)]
    , templaterExcludes :: Set Text
    }

data CompiledTemplaterSettings = CompiledTemplaterSettings
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
    where variations text = Set.toList . Set.fromList $ concatMap styleVariations (textToTemplateValues text)

expandReplacements :: [(Text, Text)] -> [(Text, Text)]
expandReplacements = concatMap expandReplacement

compileSettings :: MonadLog m => TemplaterSettings -> m CompiledTemplaterSettings
compileSettings TemplaterSettings { templaterVariables = tv, templaterExcludes = _ }
    = let replacements = expandReplacements tv in do
        logDebug $ "Veriables:" <+> align (sep [ pretty f <+> "->" <+> pretty t | (f, t) <- tv ])
        logDebug $ "Replacements:" <+> align (sep [ pretty f <+> "->" <+> pretty t | (f, t) <- replacements ])
        return CompiledTemplaterSettings { translate = replaceVariables replacements }

replaceVariables :: [(Text, Text)] -> Text -> Text
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
mapContent f (BinaryContent b) = BinaryContent b

detectEncoding :: ByteString -> FileContent
detectEncoding bs = case decodeUtf8' bs of
    Right t -> TextContent t encodeUtf8
    Left  _ -> BinaryContent bs

contentToByteString :: FileContent -> ByteString
contentToByteString (TextContent t enc)  = enc t
contentToByteString (BinaryContent bs) = bs

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
    -> m ()
copyAbsFile settings src dst = do
    logDebug $ "Copying" <+> pretty src <+> "to" <+> pretty dst
    byteContent <- liftIO $ BS.readFile (toFilePath src)
    let content = detectEncoding byteContent
        translatedContent = mapContent (translate settings) content
        translated = contentToByteString translatedContent in do
        ensureDir (parent dst)
        liftIO $ BS.writeFile (toFilePath dst) translated

copyRelFile
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => CompiledTemplaterSettings
    -> Path Abs Dir
    -> Path Abs Dir
    -> Path Rel File
    -> m ()
copyRelFile settings src dst srcFile = do
    dstFile <- translateFile settings srcFile
    copyAbsFile settings (src </> srcFile) (dst </> dstFile)

copy
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => TemplaterSettings
    -> Path Abs Dir
    -> Path Abs Dir
    -> m ()
copy settings src dst = do
    compiledSettings <- compileSettings settings
    let
        dirWalker dir subdirs files = do
            traverse_ (\f -> fileWalker (dir </> f)) files
            return $ WalkExclude (exclude subdirs)
        fileWalker = copyRelFile compiledSettings src dst
        exclude    = filter ((==) ".git/" . fromRelDir)
        in walkDirRel dirWalker src
