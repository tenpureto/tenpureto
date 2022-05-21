module Tenpureto.OrderedMapSpec
    ( spec
    ) where

import           Control.Applicative
import qualified Data.Map                      as Map
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Syd
import           Test.Syd.Hedgehog

import           Tenpureto.OrderedMap

spec :: Spec
spec = describe "OrderedMap" $ do
    it "preserves elements" $ property $ do
        list <- forAll $ genUniqueListIntString (Range.linear 0 10)
        (toList . fromList) list === list
    describe "union" $ do
        describe "preserves order" $ do
            it "concatenation" $ do
                s [(1, 'a'), (2, 'b')]
                    `union`    s [(2, 'c'), (3, 'd')]
                    `shouldBe` s [(1, 'a'), (2, 'c'), (3, 'd')]
            it "insertion" $ do
                s [(1, 'a'), (3, 'b'), (4, 'c'), (5, 'd')]
                    `union`    s [(2, 'e'), (3, 'f'), (4, 'g')]
                    `shouldBe` s
                                   [ (1, 'a')
                                   , (2, 'e')
                                   , (3, 'f')
                                   , (4, 'g')
                                   , (5, 'd')
                                   ]
            it "resolves conflicts" $ do
                s [(1, 'a'), (2, 'b'), (3, 'c')]
                    `union`    s [(3, 'd'), (4, 'e'), (1, 'f')]
                    `shouldBe` s [(2, 'b'), (3, 'd'), (4, 'e'), (1, 'f')]
    describe "show" $ do
        it "shows as fromList" $ do
            show (s [(1, 'a'), (2, 'b')])
                `stringShouldBe` "fromList [(1,'a'),(2,'b')]"

genUniqueListIntString :: (MonadGen m) => Range Int -> m [(Int, String)]
genUniqueListIntString range =
    Gen.map
            range
            (liftA2 (,)
                    (Gen.int range)
                    (Gen.string (Range.singleton 1) Gen.lower)
            )
        >>= Gen.shuffle
        .   Map.assocs

genOrderedMapIntString
    :: (MonadGen m) => Range Int -> m (OrderedMap Int String)
genOrderedMapIntString range = fromList <$> genUniqueListIntString range

s :: [(Int, Char)] -> OrderedMap Int Char
s = fromList
