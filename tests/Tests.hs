{-# language
   GeneralizedNewtypeDeriving
 , RecordWildCards
 , ConstraintKinds
 , ViewPatterns
 #-}

module Main(main) where

import Data.Set.SetInterface

import qualified Data.List as List
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
  arbitrary = RI `fmap` choose (1,500)

type Carry = RInt

toInt :: [Carry] -> [Int]
toInt = fmap unRI

equalSets :: Eq a => SetInterface f a -> [a] -> f -> Bool
equalSets SetInterface{..} ls ds =
  all (`member` ds) ls &&
  all (`List.elem` ls) (toList ds) &&
  valid ds

propConstruction :: SetInterface f Int -> [Carry] -> Bool
propConstruction it@SetInterface{..} (toInt -> xs) =
  equalSets it xs   xs' &&
  equalSets it xs'' xs'
  where
    xs'  = fromList xs
    xs'' = toList xs'

propConstruction' :: SetInterface f Int -> Carry -> Carry -> Bool
propConstruction' it@SetInterface{..} (unRI -> a) (unRI -> b) = ls == ls'
  where
    ls  = [min a b .. max a b]
    ls' = List.sort . toList . fromList $ ls

propFindMin :: SetInterface f Int -> Carry -> Carry -> Bool
propFindMin it@SetInterface{..} (unRI -> a) (unRI -> b) = mn == mn'
  where
    mn  = min a b
    mn' = findMin . fromList $ [min a b .. max a b]

propIntersection :: SetInterface f Int -> [Carry] -> Bool
propIntersection it@SetInterface{..} (toInt -> xs) =
  equalSets it i' i &&
  all (`member` i) (l `List.intersect` r) &&
  all (`notMember` i) (dl `List.union` dr)
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ List.nub xs

    dl = l List.\\ r
    dr = r List.\\ l

    i' = l `List.intersect` r
    i = (fromList l) `intersection` (fromList r)

propUnion :: SetInterface f Int -> [Carry] -> Bool
propUnion it@SetInterface{..} (toInt -> xs) = equalSets it u i
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ List.nub xs

    i = (fromList l) `union` (fromList r)
    u = l `List.union` r

propDifference :: SetInterface f Int -> [Carry] -> Bool
propDifference it@SetInterface{..} (toInt -> xs) = equalSets it l l'
  where
    n = length xs
    (l, r) = splitAt (n `div` 2) $ List.nub xs
    u = l `List.union` r

    l' = (fromList u) `difference` (fromList r)

main :: IO ()
main = defaultMain tests
  where
    tests :: [Test]
    tests = [ testGroup "Data.Set.Diet"       testDiet
            , testGroup "Data.Set.WordBitSet" testWordBitSet
            ]

    testOpt = mempty { topt_maximum_generated_tests = Just 10000 }

    testDiet :: [Test]
    testDiet =
      map (plusTestOptions testOpt)
      [ testProperty "test intersection"  (propIntersection  dietInterface)
      , testProperty "test construction1" (propConstruction  dietInterface)
      , testProperty "test construction2" (propConstruction' dietInterface)
      , testProperty "test find minimum"  (propFindMin       dietInterface)
      , testProperty "test difference"    (propDifference    dietInterface)
      , testProperty "test union"         (propUnion         dietInterface)
      ]

    testWordBitSet :: [Test]
    testWordBitSet =
      map (plusTestOptions testOpt)
      [ testProperty "test intersection"  (propIntersection  wordBitSetInterface)
      , testProperty "test construction1" (propConstruction  wordBitSetInterface)
      , testProperty "test construction2" (propConstruction' wordBitSetInterface)
      , testProperty "test find minimum"  (propFindMin       wordBitSetInterface)
      , testProperty "test difference"    (propDifference    wordBitSetInterface)
      , testProperty "test union"         (propUnion         wordBitSetInterface)
      ]
