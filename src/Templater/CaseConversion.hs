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
    deriving (Show, Eq, Ord, Generic)

data WordSeparator =  NoSeparator | SingleSeparator Char
    deriving (Show, Eq, Ord)

data TemplateValue = MultiWordTemplateValue WordCase WordCase WordSeparator [Text]
                   | SingleWordTemplateValue WordCase Text
                   | LiteralTemplateValue Text
    deriving (Show, Eq, Ord)

allWordCases :: [WordCase]
allWordCases = [LowerCase, UpperCase, TitleCase, MixedCase]

validSeparators :: String
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

wordCasesApply :: WordCase -> WordCase -> [Text] -> [Text]
wordCasesApply fwc owc =
    zipWith ($) (wordCaseApply fwc : repeat (wordCaseApply owc))

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
    T.intercalate (separator s) (wordCasesApply fwc owc tt)

sameStyle :: TemplateValue -> TemplateValue -> Bool
sameStyle (MultiWordTemplateValue fwc owc s _) (MultiWordTemplateValue fwc' owc' s' _)
    = fwc == fwc' && owc == owc' && s == s'
sameStyle (SingleWordTemplateValue c _) (SingleWordTemplateValue c' _) =
    c == c'
sameStyle (LiteralTemplateValue _) (LiteralTemplateValue _) = True
sameStyle _                        _                        = False

wordCaseVariations :: WordCase -> [WordCase]
wordCaseVariations MixedCase = [LowerCase, UpperCase, TitleCase, MixedCase]
wordCaseVariations _         = [LowerCase, UpperCase, TitleCase]

styleVariations :: TemplateValue -> [TemplateValue]
styleVariations (MultiWordTemplateValue fwc owc _ tt) =
    [ MultiWordTemplateValue fwc' owc' s' (wordCasesApply fwc' owc' tt)
    | fwc' <- wordCaseVariations fwc
    , owc' <- wordCaseVariations owc
    , s'   <- allWordSeparators
    ]
styleVariations (SingleWordTemplateValue c t) =
    LiteralTemplateValue t
        : [ SingleWordTemplateValue c' (wordCaseApply c' t)
          | c' <- wordCaseVariations c
          ]
styleVariations (LiteralTemplateValue t) = [LiteralTemplateValue t]
