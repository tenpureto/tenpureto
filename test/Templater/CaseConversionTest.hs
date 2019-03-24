{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Templater.CaseConversionTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.SmallCheck
import           Test.SmallCheck.Series

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.ICU                 as ICU
import           Templater.CaseConversion
import           GHC.Generics

assertElem :: (Show a, Eq a) => a -> [a] -> Assertion
assertElem a b = assertBool
    ("expected: " ++ show a ++ " to be an element of: " ++ show b)
    (a `elem` b)

scprop_wordCasePatternsNotEmpty :: WordCase -> Bool
scprop_wordCasePatternsNotEmpty wc =
    let (fw, ow) = wordCasePatterns wc
        fwp      = ICU.regex [] fw
        owp      = ICU.regex [] ow
    in  isNothing (ICU.find fwp "") && isNothing (ICU.find owp "")

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
        , ("aaa-bbb-ccc"  , LowerCase , SingleSeparator '-')
        , ("AAA_BBB_CCC"  , UpperCase , SingleSeparator '_')
        , ("aaabbbccc"    , LowerCase , NoSeparator)
        , ("Aaabbbccc"    , PascalCase, NoSeparator)
        , ("Aaa bbb ccc"  , MixedCase , SingleSeparator ' ')
        , ("aaa/bbb/ccc"  , LowerCase , SingleSeparator '/')
        , ("aaa\\bbb\\ccc", LowerCase , SingleSeparator '\\')
        , ("aaa.bbb.ccc"  , LowerCase , SingleSeparator '.')
        ]

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

instance Monad m => Serial m WordCase

newtype TestChar = TestChar { testChar :: Char }

instance Monad m => Serial m TestChar where
    series = generate
        (const $ map TestChar (['a', 'A', '1', '!'] ++ validSeparators))

newtype TestText = TestText { testChars :: [TestChar] } deriving (Generic)

instance Monad m => Serial m TestText
instance Show TestText where
    show testText = "\"" ++ map testChar (testChars testText) ++ "\""
