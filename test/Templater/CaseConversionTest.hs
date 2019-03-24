{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Templater.CaseConversionTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.SmallCheck
import           Test.SmallCheck.Series

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Templater.CaseConversion
import           GHC.Generics

assertElem :: (Show a, Eq a) => a -> [a] -> Assertion
assertElem a b = assertBool
    ("expected: " ++ show a ++ " to be an element of: " ++ show b)
    (a `elem` b)

test_detectValueStyle :: [TestTree]
test_detectValueStyle = concatMap tests cases  where
    tests :: (Text, WordCase, WordSeparator) -> [TestTree]
    tests (str, wc, ws) =
        let names = textToTemplateValues str
        in  [ testCase (T.unpack str <> " is a " <> show wc)
                       (Just wc `assertElem` map wordCase names)
            , testCase (T.unpack str <> " is a " <> show ws)
                       (Just ws `assertElem` map wordSeparator names)
            ]
    cases =
        [ ("aaaBbbCcc"    , CamelCase , NoSeparator)
        , ("AaaBbbCcc"    , PascalCase, NoSeparator)
        , ("aaa-bbb-ccc"  , LowerCase , DashSeparator)
        , ("AAA_BBB_CCC"  , UpperCase , UnderscoreSeparator)
        , ("aaabbbccc"    , LowerCase , NoSeparator)
        , ("Aaabbbccc"    , PascalCase, NoSeparator)
        , ("Aaa bbb ccc"  , MixedCase , SpaceSeparator)
        , ("aaa/bbb/ccc"  , LowerCase , SlashSeparator)
        , ("aaa\\bbb\\ccc", LowerCase , BackslashSeparator)
        ]

newtype TestChar = TestChar { testChar :: Char }

instance Monad m => Serial m TestChar where
    series =
        generate
            (const $ map TestChar ['a', 'A', '1', ' ', '-', '_', '/', '\\', '!']
            )

newtype TestText = TestText { testChars :: [TestChar] } deriving (Generic)

instance Monad m => Serial m TestText
instance Show TestText where
    show testText = "\"" ++ map testChar (testChars testText) ++ "\""

scprop_templateValueTextReversible :: TestText -> Either Reason Reason
scprop_templateValueTextReversible testText =
    let text   = T.pack $ show testText
        values = textToTemplateValues text
        reversible value =
                let reversed = templateValueText value
                in  if reversed == text
                        then Right ()
                        else Left
                            (  T.unpack reversed
                            ++ " is not equal to "
                            ++ T.unpack text
                            ++ " when represented as "
                            ++ show value
                            )
        results = mapM_ reversible values
    in  results >> return (T.unpack text ++ " is reversible")
