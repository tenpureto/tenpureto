{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.TemplaterSpec
    ( spec
    ) where

import qualified Control.Exception             as E
import qualified Data.Set                      as Set
import           Path
import           Polysemy
import           Polysemy.Error
import           Test.Syd

import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Logging
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.Templater

spec :: Spec
spec = do
    describe "translate" $ do
        describe "multi-word to single-word"
            $ let Right settings = runPure $ compileSettings TemplaterSettings
                      { templaterFromVariables = OrderedMap.singleton
                                                     "A"
                                                     "bbb-ccc-ddd"
                      , templaterToVariables   = OrderedMap.singleton "A" "xxx"
                      , templaterExcludes      = Set.empty
                      }
              in  do
                      it "should replace in text" $ do
                          translate settings "a Bbb ccc ddd e"
                              `shouldBe` "a Xxx e"
        describe "single-word to multi-word"
            $ let Right settings = runPure $ compileSettings TemplaterSettings
                      { templaterFromVariables = OrderedMap.singleton "A" "bbb"
                      , templaterToVariables   = OrderedMap.singleton
                                                     "A"
                                                     "xxx-yyy"
                      , templaterExcludes      = Set.empty
                      }
              in  do
                      it "should replace in text" $ do
                          translate settings "a bbb e" `shouldBe` "a xxx-yyy e"
    describe "translateFile" $ do
        describe "simple case"
            $ let Right settings = runPure $ compileSettings TemplaterSettings
                      { templaterFromVariables = OrderedMap.singleton
                                                     "A"
                                                     "bbb-ccc-ddd"
                      , templaterToVariables   = OrderedMap.singleton
                                                     "A"
                                                     "xxx-yyy"
                      , templaterExcludes      = Set.empty
                      }
                  mv = runPure . translateFile settings
              in  do
                      it "should replace in file name" $ do
                          mv [relfile|a-bbb-ccc-ddd-e.txt|]
                              `shouldBe` Right [relfile|a-xxx-yyy-e.txt|]
                      it "should replace in path" $ do
                          mv [relfile|a/bbb/ccc/ddd/e/f.txt|]
                              `shouldBe` Right [relfile|a/xxx/yyy/e/f.txt|]
        describe "to-variable has a space"
            $ let Right settings = runPure $ compileSettings TemplaterSettings
                      { templaterFromVariables = OrderedMap.singleton
                                                     "A"
                                                     "bbb-ccc-ddd"
                      , templaterToVariables   = OrderedMap.singleton
                                                     "A"
                                                     "xxx yyy"
                      , templaterExcludes      = Set.empty
                      }
                  mv = runPure . translateFile settings
              in  do
                      it "should replace in file name" $ do
                          mv [relfile|a-bbb-ccc-ddd-e.txt|]
                              `shouldBe` Right [relfile|a-xxx-yyy-e.txt|]
                      it "should replace in path" $ do
                          mv [relfile|a/bbb/ccc/ddd/e/f.txt|]
                              `shouldBe` Right [relfile|a/xxx/yyy/e/f.txt|]
        describe "to-variable has upper-case characters"
            $ let Right settings = runPure $ compileSettings TemplaterSettings
                      { templaterFromVariables = OrderedMap.singleton
                                                     "A"
                                                     "bbb-ccc-ddd"
                      , templaterToVariables   = OrderedMap.singleton
                                                     "A"
                                                     "Xxx Yyy"
                      , templaterExcludes      = Set.empty
                      }
                  mv = runPure . translateFile settings
              in  do
                      it "should replace in file name" $ do
                          mv [relfile|a-bbb-ccc-ddd-e.txt|]
                              `shouldBe` Right [relfile|a-xxx-yyy-e.txt|]
                      it "should replace in path" $ do
                          mv [relfile|a/bbb/ccc/ddd/e/f.txt|]
                              `shouldBe` Right [relfile|a/xxx/yyy/e/f.txt|]
                      it "should replace in path" $ do
                          mv [relfile|a/bbb-ccc-ddd/e/f.txt|]
                              `shouldBe` Right [relfile|a/xxx-yyy/e/f.txt|]
    describe "excludes"
        $ let assertMatches a b = assertBool
                  (  "expected pattern: "
                  ++ show a
                  ++ " to match file: "
                  ++ show b
                  )
                  (compileExcludes (Set.singleton a) b)
              assertNotMatches a b =
                  unless (not $ compileExcludes (Set.singleton a) b)
                      $ expectationFailure
                            (  "expected pattern: "
                            ++ show a
                            ++ " to not match file: "
                            ++ show b
                            )
            --   assertNotMatches a b = assertBool
            --       (  "expected pattern: "
            --       ++ show a
            --       ++ " to not match file: "
            --       ++ show b
            --       )
            --       (not $ compileExcludes (Set.singleton a) b)
          in  do
                  describe "wildcard" $ do
                      it "should match a file" $ do
                          assertMatches "*" [relfile|a|]
                      it "should match a file in a dir" $ do
                          assertMatches "*" [relfile|a/b|]

                  describe "double wildcard" $ do
                      it "should match zero dirs" $ do
                          assertMatches "/a/**/b" [relfile|a/b|]
                      it "should match one dir" $ do
                          assertMatches "/a/**/b" [relfile|a/c/b|]
                      it "should match two dirs" $ do
                          ssertMatches "/a/**/b" [relfile|a/c/d/b|]

                  describe "root dir" $ do
                      it "should match a file in a dir" $ do
                          assertMatches "/a/" [relfile|a/b|]
                      it "should match a file in a subdir" $ do
                          assertMatches "/a/" [relfile|a/b/c|]
                      it "should not match a file" $ do
                          assertNotMatches "/a/" [relfile|a|]
                  describe "root path" $ do
                      it "should match a file in a dir" $ do
                          assertMatches "/a" [relfile|a/b|]
                      it "should match a file in a subdir" $ do
                          assertMatches "/a" [relfile|a/b/c|]
                      it "should match a file" $ do
                          assertMatches "/a" [relfile|a|]
                  describe "dir" $ do
                      it "should match a dir" $ do
                          assertMatches "a/" [relfile|a/b|]
                      it "should not match a file" $ do
                          assertNotMatches "a/" [relfile|b/a|]
                      it "should match a file in a dir" $ do
                          assertMatches "a/" [relfile|a/b|]
                      it "should match a file in a dir when starts with a dot"
                          $ do
                                assertMatches ".a/" [relfile|.a/b|]
                      it
                              "should match a file in a subdir when starts with a dot"
                          $ do
                                assertMatches ".a/" [relfile|.a/b/c|]
                  describe "path" $ do
                      it "should match a file in a dir" $ do
                          assertMatches "a" [relfile|b/a|]
                      it "should match a file" $ do
                          assertMatches "a" [relfile|a/b|]
                      it "should match a file in a dir" $ do
                          assertMatches "a" [relfile|a/b|]
                      it "should match a file in a dir when starts with a dot"
                          $ do
                                assertMatches ".a" [relfile|.a/b|]
                  it "should not match invalid patterns" $ do
                      assertNotMatches "[" [relfile|a|]

data NotImplemented = NotImplemented
    deriving (Eq, Show)
instance E.Exception NotImplemented

runFileSystemPure
    :: Sem (FileSystem ': r) a -> Sem (Error E.SomeException ': r) a
runFileSystemPure = reinterpret $ \case
    ParseRelFile             filePath -> sendE $ Path.parseRelFile filePath
    EnsureDir                _        -> throwE NotImplemented
    EnsureEmptyDir           _        -> throwE NotImplemented
    ResolveDir               _        -> throwE NotImplemented
    ResolveFile              _        -> throwE NotImplemented
    IsSymlink                _        -> throwE NotImplemented
    GetSymbolicLinkDirTarget _        -> throwE NotImplemented
    CreateDirectoryLink _ _           -> throwE NotImplemented
    CopyPermissions     _ _           -> throwE NotImplemented
    RenameFile          _ _           -> throwE NotImplemented
    RemoveFile                _       -> throwE NotImplemented
    ReadFileAsByteString      _       -> throwE NotImplemented
    ReadFileAsMaybeByteString _       -> throwE NotImplemented
    WriteFileAsByteString _ _         -> throwE NotImplemented
    OpenBinaryTempFile    _ _         -> throwE NotImplemented
    CreateSystemTempDir _             -> throwE NotImplemented
    RemoveDirRecur      _             -> throwE NotImplemented
    HPutByteString _ _                -> throwE NotImplemented
    HClose _                          -> throwE NotImplemented
  where
    sendE :: Either e a -> Sem (Error e ': r) a
    sendE = either throw return
    throwE :: E.Exception e => e -> Sem (Error E.SomeException ': r) a
    throwE e = throw $ E.SomeException e

runPure :: Sem '[Logging , FileSystem , Error String] a -> Either String a
runPure = run . runError . mapError show . runFileSystemPure . runNoLogging
