module Tenpureto.OrderedSetSpec
    ( spec
    ) where

import           Data.List                      ( isSubsequenceOf
                                                , sort
                                                )
import qualified Data.Set                      as Set
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Syd
import           Test.Syd.Hedgehog

import           Tenpureto.OrderedSet

spec :: Spec
spec = describe "OrderedSet" $ do
    it "preserves elements" $ property $ do
        list <- forAll $ genUniqueList (Range.linear 0 10)
                                       (Gen.int Range.constantBounded)
        (toList . fromList) list === list
    describe "intersection" $ do
        describe "preserves order" $ do
            it "infix" $ do
                s [1, 2, 3, 4] `intersection` s [2, 3] `shouldBe` s [2, 3]
            it "subsequence" $ do
                s [1, 2, 3, 5] `intersection` s [2, 4, 5, 6] `shouldBe` s [2, 5]
        it "resolves conflicts" $ do
            s [1, 2, 3] `intersection` s [3, 2, 4] `shouldBe` s [3, 2]
        it "preserves elements" $ property $ do
            a <- forAll $ genUniqueListInt (Range.linear 0 10)
            b <- forAll $ genUniqueListInt (Range.linear 0 10)
            let elements = toList (fromList a `intersection` fromList b)
            let
                expected = Set.toList
                    (Set.fromList a `Set.intersection` Set.fromList b)
            sort elements === expected
        it "preserves right-side order" $ property $ do
            a <- forAll $ genUniqueListInt (Range.linear 0 10)
            b <- forAll $ genUniqueListInt (Range.linear 0 10)
            let elements = toList (fromList a `intersection` fromList b)
            diff elements isSubsequenceOf b
    describe "union" $ do
        describe "preserves order" $ do
            it "concatenation" $ do
                s [1, 2] `union` s [2, 3] `shouldBe` s [1, 2, 3]
            it "insertion" $ do
                s [1, 3, 4, 5] `union` s [2, 3, 4] `shouldBe` s [1, 2, 3, 4, 5]
        it "resolves conflicts" $ do
            s [1, 2, 3] `union` s [3, 4, 1] `shouldBe` s [2, 3, 4, 1]
        it "preserves elements" $ property $ do
            a <- forAll $ genUniqueListInt (Range.linear 0 10)
            b <- forAll $ genUniqueListInt (Range.linear 0 10)
            let elements = toList (fromList a `union` fromList b)
            let expected = Set.toList (Set.fromList a <> Set.fromList b)
            sort elements === expected
        it "preserves right-side order" $ property $ do
            a <- forAll $ genUniqueListInt (Range.linear 0 10)
            b <- forAll $ genUniqueListInt (Range.linear 0 10)
            let elements = toList (fromList a `union` fromList b)
            diff b isSubsequenceOf elements

genUniqueList :: (MonadGen m, Eq a, Ord a) => Range Int -> m a -> m [a]
genUniqueList range gen = Gen.set range gen >>= Gen.shuffle . Set.toList

genOrderedSet
    :: (MonadGen m, Eq a, Ord a) => Range Int -> m a -> m (OrderedSet a)
genOrderedSet range gen = fromList <$> genUniqueList range gen

genUniqueListInt :: MonadGen m => Range Int -> m [Int]
genUniqueListInt range = genUniqueList range (Gen.int range)

genOrderedSetInt :: MonadGen m => Range Int -> m (OrderedSet Int)
genOrderedSetInt range = genOrderedSet range (Gen.int range)

s :: [Int] -> OrderedSet Int
s = fromList
