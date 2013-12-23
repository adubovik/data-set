{-# language
   GeneralizedNewtypeDeriving
 #-}

module Main(main) where

import Data.Set.Diet(Diet)
import qualified Data.Set.Diet as DI

import Data.List
import Data.Monoid(mempty)
import Test.QuickCheck
import Test.Framework (Test, testGroup, defaultMain, plusTestOptions)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Options

newtype RInt = RI { unRI :: Int }
  deriving (Eq, Ord, Enum)

instance Show RInt where
  show = show . unRI

instance Arbitrary RInt where
  arbitrary = RI `fmap` choose (1,50)

type Carry = RInt

equalSets :: [Carry] -> Diet Carry -> Bool
equalSets ls ds =
  all (`DI.member` ds) ls &&
  all (`elem` ls) (DI.toList ds) &&
  DI.valid ds

propConstruction :: [Carry] -> Bool
propConstruction xs =
  equalSets xs d && xs' == xs''
  where
    d    = DI.fromList xs
    xs'  = DI.toAscList d
    xs'' = nub $ sort xs

propIntersection :: [Carry] -> Bool
propIntersection xs =
  equalSets i' i &&
  all (`DI.member` i) (l `intersect` r) &&
  all (`DI.notMember` i) (dl `union` dr)
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    dl = l \\ r
    dr = r \\ l

    i' = l `intersect` r
    i = (DI.fromList l) `DI.intersection` (DI.fromList r)

propUnion :: [Carry] -> Bool
propUnion xs = equalSets u i
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    i :: Diet Carry
    i = (DI.fromList l) `DI.union` (DI.fromList r)

    u = l `union` r

propDifference :: [Carry] -> Bool
propDifference xs = equalSets l l'
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs
    u = l `union` r

    l' :: Diet Carry
    l' = (DI.fromList u) `DI.difference` (DI.fromList r)

main :: IO ()
main = defaultMain tests
  where
    tests :: [Test]
    tests = [ testGroup "Data.Diet" testDiet ]

    testOpt = mempty { topt_maximum_generated_tests = Just 10000 }

    testDiet :: [Test]
    testDiet =
      map (plusTestOptions testOpt)
      [ testProperty "test intersection" propIntersection
      , testProperty "test construction" propConstruction
      , testProperty "test difference" propDifference
      , testProperty "test union" propUnion
      ]
