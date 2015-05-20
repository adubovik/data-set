{-# language
   ExistentialQuantification
 , Rank2Types
 , ConstraintKinds
 , RecordWildCards
 , FlexibleContexts
 , CPP
 #-}

module Main (main) where

import Data.List (foldl')
import Control.DeepSeq (NFData(..))
import Criterion.Main (defaultMain, bench, bgroup, whnf, Benchmark)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

import Data.Set.Interface
import Data.Set.Diet(DietC)

data B = forall a. NFData a => B a

instance NFData B where
    rnf (B b) = rnf b

instance (NFData f, NFData a) => NFData (BenchData f a) where
  rnf (BenchData {..}) = rnf [big, small] `seq`
                         rnf [bigRnd, smallRnd] `seq`
                         rnf [bigAsc, smallAsc] `seq`
                         rnf bigSize `seq`
                         rnf title

data BenchData f a = BenchData
  { big, small :: f
  , bigRnd, smallRnd :: [a]
  , bigAsc, smallAsc :: [a]
  , bigSize :: Int
  , title :: String
  }

mkBench :: Interface f Int -> BenchData f Int -> Benchmark
mkBench it@Interface{..} BenchData{..} =
  bgroup (confName ++ "/" ++ title)
  [ bench "fromList"     (whnf fromList bigRnd)
  , bench "toList"       (whnf toList big)
  , bench "singleton"    (whnf singleton bigSize)

    -- Weak spot of Diet, because of lack of rebalancing
  , bench "insert asc"   (whnf (inserts it bigAsc) empty)
  , bench "insert rnd"   (whnf (inserts it bigRnd) empty)

    -- Weak spot of Diet in solid input case - to many split should be performed I suppose
  , bench "delete"       (whnf (deletes it bigRnd) big)
  , bench "notMember"    (whnf (notMembers it bigRnd) big)
  , bench "member"       (whnf (members it bigRnd) big)
  , bench "intersection" (whnf (intersection small) big)
  , bench "difference"   (whnf (difference small) big)

    -- Weak spot of Diet, because of lack of cool hedge-union algorithm
  , bench "union"        (whnf (union small) big)
  , bench "map"          (whnf (mapc (+ bigSize)) big)
  ]

main :: IO ()
main = do
  let n :: Int
      n = 16 * 4096

      eSolidBig = [1..n]
      eSolidSmall = [1..n `div` 2]

      eSparseBig = map (*3) eSolidBig
      eSparseSmall = map (*3) eSolidSmall

  let r = mkStdGen 42
      bdSparse Interface{..} = BenchData
        { small    = fromList eSparseSmall
        , big      = fromList eSparseBig
        , smallRnd = shuffle' eSparseSmall (n `div` 2) r
        , bigRnd   = shuffle' eSparseBig n r
        , smallAsc = eSparseSmall
        , bigAsc   = eSparseBig
        , title    = "Sparse_input"
        , bigSize  = n
        }

      bdSolid Interface{..} = BenchData
        { small    = fromList eSolidSmall
        , big      = fromList eSolidBig
        , smallRnd = shuffle' eSolidSmall (n `div` 2) r
        , bigRnd   = shuffle' eSolidBig n r
        , smallAsc = eSparseSmall
        , bigAsc   = eSparseBig
        , title    = "Solid_input"
        , bigSize  = n
        }

  let dietSolid    = bdSolid  dietInterface
      dietSparse   = bdSparse dietInterface
      setSolid     = bdSolid  setInterface
      setSparse    = bdSparse setInterface
      intSetSolid  = bdSolid  intSetInterface
      intSetSparse = bdSparse intSetInterface
      wordBitSetSparse = bdSparse wordBitSetInterface
      wordBitSetSolid  = bdSolid  wordBitSetInterface

#ifdef BITSET
      bitSetSolid  = bdSolid  bitSetInterface
      bitSetSparse = bdSparse bitSetInterface
#endif

  return $ rnf [ B dietSolid       , B dietSparse
               , B setSolid        , B setSparse
               , B intSetSolid     , B intSetSparse
               , B wordBitSetSolid , B wordBitSetSparse
               ]
#ifdef BITSET
                ++ [B bitSetSolid , B bitSetSparse]
#endif

  defaultMain
    [ mkBench setInterface setSparse
    --  mkBench intSetInterface intSetSparse
#ifdef BITSET
    -- , mkBench bitSetInterface bitSetSparse
#endif
    , mkBench dietInterface dietSparse
    --, mkBench wordBitSetInterface wordBitSetSparse

    , mkBench setInterface setSolid
    -- ,  mkBench intSetInterface intSetSolid
#ifdef BITSET
    -- , mkBench bitSetInterface bitSetSolid
#endif
    , mkBench dietInterface dietSolid
    -- , mkBench wordBitSetInterface wordBitSetSolid
    ]

members :: DietC a => Interface f a -> [a] -> f -> Bool
members Interface{..} xs bs = all (\x -> member x bs) xs

notMembers :: DietC a => Interface f a -> [a] -> f -> Bool
notMembers Interface{..} xs bs = any (\x -> notMember x bs) xs

inserts :: DietC a => Interface f a -> [a] -> f -> f
inserts Interface{..} xs bs0 = foldl' (\bs x -> insert x bs) bs0 xs

deletes :: DietC a => Interface f a -> [a] -> f -> f
deletes Interface{..} xs bs0 = foldl' (\bs x -> delete x bs) bs0 xs
