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
import           Data.Foldable
import           Templater.CaseConversion
import           GHC.Generics

assertElem :: (Show a, Eq a) => a -> [a] -> Assertion
assertElem a b = assertBool
  ("expected: " ++ show a ++ " to be an element of: " ++ show b)
  (a `elem` b)

scprop_wordCasePatternsNotEmpty :: WordCase -> Bool
scprop_wordCasePatternsNotEmpty wc =
  isNothing $ ICU.find (ICU.regex [] $ wordCasePattern wc) ""

test_detectValueStyle :: [TestTree]
test_detectValueStyle = fmap runTest cases
 where
  runTest :: (Text, TemplateValue) -> TestTree
  runTest (str, tw) =
    let values = textToTemplateValues str
    in  testCase (T.unpack str <> " is a " <> show tw) (tw `assertElem` values)
  cases =
    [ ( "aaaBbbCcc"
      , MultiWordTemplateValue LowerCase
                               TitleCase
                               NoSeparator
                               ["aaa", "Bbb", "Ccc"]
      )
    , ( "AaaBbbCcc"
      , MultiWordTemplateValue TitleCase
                               TitleCase
                               NoSeparator
                               ["Aaa", "Bbb", "Ccc"]
      )
    , ( "aaa-bbb-ccc"
      , MultiWordTemplateValue LowerCase
                               LowerCase
                               (SingleSeparator '-')
                               ["aaa", "bbb", "ccc"]
      )
    , ( "AAA_BBB_CCC"
      , MultiWordTemplateValue UpperCase
                               UpperCase
                               (SingleSeparator '_')
                               ["AAA", "BBB", "CCC"]
      )
    , ( "Aaa bbb ccc"
      , MultiWordTemplateValue TitleCase
                               LowerCase
                               (SingleSeparator ' ')
                               ["Aaa", "bbb", "ccc"]
      )
    , ( "aaa/bbb/ccc"
      , MultiWordTemplateValue LowerCase
                               LowerCase
                               (SingleSeparator '/')
                               ["aaa", "bbb", "ccc"]
      )
    , ( "aaa\\bbb\\ccc"
      , MultiWordTemplateValue LowerCase
                               LowerCase
                               (SingleSeparator '\\')
                               ["aaa", "bbb", "ccc"]
      )
    , ( "aaa.bbb.ccc"
      , MultiWordTemplateValue LowerCase
                               LowerCase
                               (SingleSeparator '.')
                               ["aaa", "bbb", "ccc"]
      )
    , ("aaabbbccc", SingleWordTemplateValue LowerCase "aaabbbccc")
    , ("Aaabbbccc", SingleWordTemplateValue TitleCase "Aaabbbccc")
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
      results = traverse_ reversible values
  in  results >> return (T.unpack text ++ " is reversible")

scprop_multiWordTemplateValueLength :: TestText -> Either Reason Reason
scprop_multiWordTemplateValueLength testText =
  let text   = T.pack $ show testText
      values = textToTemplateValues text
      valid (MultiWordTemplateValue _ _ _ tt) = if length tt >= 2
        then Right ()
        else Left ("is a multi-word with " ++ show tt ++ " words")
      valid _ = Right ()
      results = traverse_ valid values
  in  results >> return (T.unpack text ++ " is reversible")

instance Monad m => Serial m WordCase

newtype TestChar = TestChar { testChar :: Char }

instance Monad m => Serial m TestChar where
  series =
    generate (const $ map TestChar (['a', 'A', '1', '!'] ++ validSeparators))

newtype TestText = TestText { testChars :: [TestChar] } deriving (Generic)

instance Monad m => Serial m TestText
instance Show TestText where
  show testText = map testChar (testChars testText)
