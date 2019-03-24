{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Templater.CaseConversion where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Maybe
import           Data.Functor
import           Data.Text.ICU                  ( Regex )
import qualified Data.Text.ICU                 as ICU
import           GHC.Generics

data WordCase = LowerCase | UpperCase | TitleCase | MixedCase
    deriving (Show, Eq, Generic)

data WordSeparator =  NoSeparator | SingleSeparator Char
    deriving (Show, Eq)

data TemplateValue = MultiWordTemplateValue WordCase WordCase WordSeparator [Text]
                   | SingleWordTemplateValue WordCase Text
                   | LiteralTemplateValue Text
    deriving (Show, Eq)

allWordCases :: [WordCase]
allWordCases = [LowerCase, UpperCase, TitleCase, MixedCase]

validSeparators :: [Char]
validSeparators = [' ', '-', '_', '/', '\\', '.']

allWordSeparators :: [WordSeparator]
allWordSeparators = NoSeparator : [ SingleSeparator c | c <- validSeparators ]

wordCasePattern :: WordCase -> Text
wordCasePattern LowerCase = "[:lower:]+"
wordCasePattern UpperCase = "[:upper:]+"
wordCasePattern TitleCase = "[:upper:][:lower:]*"
wordCasePattern MixedCase = "(?:[:upper:]|[:lower:])+"

wordCaseApply :: WordCase -> Text -> Text
wordCaseApply LowerCase = T.toLower
wordCaseApply UpperCase = T.toUpper
wordCaseApply TitleCase = T.toTitle
wordCaseApply MixedCase = id

separator :: WordSeparator -> Text
separator NoSeparator         = ""
separator (SingleSeparator c) = T.singleton c

capitalLetterPattern :: Regex
capitalLetterPattern = ICU.regex [] ".(?=[:upper:])|$"

splitOnR :: ICU.Regex -> Text -> [Text]
splitOnR regex text = ICU.findAll regex text
    <&> \m -> ICU.span m <> fromMaybe T.empty (ICU.group 0 m)

splitWords :: WordCase -> WordCase -> WordSeparator -> Maybe (Text -> [Text])
splitWords _ _ (SingleSeparator c) = Just $ T.splitOn (T.singleton c)
splitWords LowerCase TitleCase NoSeparator =
    Just $ splitOnR capitalLetterPattern
splitWords TitleCase TitleCase NoSeparator =
    Just $ splitOnR capitalLetterPattern
splitWords _ _ NoSeparator = Nothing

multiWordPatterns :: [(Regex, Text -> TemplateValue)]
multiWordPatterns =
    [ (build fwc owc s, MultiWordTemplateValue fwc owc s . spl)
    | fwc <- allWordCases
    , owc <- allWordCases
    , s   <- allWordSeparators
    , spl <- maybeToList $ splitWords fwc owc s
    ]  where
    build fwc owc s = ICU.regex
        []
        (  "^"
        <> wordCasePattern fwc
        <> "(?:\\Q"
        <> separator s
        <> "\\E"
        <> wordCasePattern owc
        <> ")+$"
        )

singleWordPatterns :: [(Regex, Text -> TemplateValue)]
singleWordPatterns =
    [ (build c, SingleWordTemplateValue c) | c <- allWordCases ]
    where build c = ICU.regex [] ("^" <> wordCasePattern c <> "$")

textToTemplateValues :: Text -> [TemplateValue]
textToTemplateValues text =
    LiteralTemplateValue text
        :  mapMaybe (uncurry f) singleWordPatterns
        ++ mapMaybe (uncurry f) multiWordPatterns
    where f p g = ICU.find p text $> g text

templateValueText :: TemplateValue -> Text
templateValueText (LiteralTemplateValue t     ) = t
templateValueText (SingleWordTemplateValue c t) = wordCaseApply c t
templateValueText (MultiWordTemplateValue fwc owc s tt) =
    let wc = wordCaseApply fwc : repeat (wordCaseApply owc)
    in  T.intercalate (separator s) (zipWith ($) wc tt)
