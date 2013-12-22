{-# language
   GeneralizedNewtypeDeriving
 #-}

module Main(main) where

import Data.Diet(Diet)
import qualified Data.Diet as D

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
  all (`D.member` ds) ls &&
  all (`elem` ls) (D.toList ds) &&
  D.valid ds

propConstruction :: [Carry] -> Bool
propConstruction xs =
  equalSets xs d && xs' == xs''
  where
    d    = D.fromList xs
    xs'  = D.toAscList d
    xs'' = nub $ sort xs

propIntersection :: [Carry] -> Bool
propIntersection xs =
  equalSets i' i &&
  all (`D.member` i) (l `intersect` r) &&
  all (`D.notMember` i) (dl `union` dr)
  where    
    n = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    dl = l \\ r
    dr = r \\ l

    i' = l `intersect` r 
    i = (D.fromList l) `D.intersection` (D.fromList r)

propUnion :: [Carry] -> Bool
propUnion xs = equalSets u i
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    i :: Diet Carry
    i = (D.fromList l) `D.union` (D.fromList r)

    u = l `union` r

propDifference :: [Carry] -> Bool
propDifference xs = equalSets l l'
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs
    u = l `union` r

    l' :: Diet Carry
    l' = (D.fromList u) `D.difference` (D.fromList r)

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
