{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Templater where

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import           Data.Map                       ( Map )
import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Logging
import           Path
import           Path.IO

data TemplaterSettings = TemplaterSettings
    { templaterVariables :: Map Text Text
    , templaterExcludes :: Set Text
    }

data TemplaterException = TemplaterException
    deriving (Show, Exception)

copy
    :: (MonadIO m, MonadThrow m, MonadLog m)
    => TemplaterSettings
    -> FilePath
    -> FilePath
    -> m ()
copy settings src dst = do
    srcDir <- parseAbsDir src
    dstDir <- parseAbsDir dst
    let
        copyFile file = let srcFile = srcDir </> file
                            dstFile = dstDir </> file in do
            logDebug $ "Copying" <+> pretty srcFile <+> "to" <+> pretty dstFile
            content <- liftIO $ BS.readFile (toFilePath srcFile)
            ensureDir (parent dstFile)
            liftIO $ BS.writeFile (toFilePath dstFile) content
        dirWalker dir subdirs files = do
            traverse_ (\f -> fileWalker (dir </> f)) files
            return $ WalkExclude (exclude subdirs)
        fileWalker = copyFile
        exclude    = filter ((==) ".git/" . fromRelDir)
        in walkDirRel dirWalker srcDir

instance Pretty (Path a t) where
    pretty = pretty . toFilePath
