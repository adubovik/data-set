{-# LANGUAGE
   ExistentialQuantification
 , Rank2Types
 , ConstraintKinds
 , RecordWildCards
 , FlexibleContexts
 #-}

module Main (main) where

import Data.List (foldl')

import Control.DeepSeq (NFData(..))
import Criterion.Main (defaultMain, bench, bgroup, whnf, Benchmark)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import qualified Data.Set as ST
import qualified Data.Diet as DI

data B = forall a. NFData a => B a

instance NFData B where
    rnf (B b) = rnf b

instance (NFData (f a), NFData a) => NFData (BenchData f a) where
  rnf (BenchData {..}) = rnf [big, small] `seq`
                         rnf [bigRnd, smallRnd] `seq`
                         rnf bigSize `seq`
                         rnf title

data BenchData f a = BenchData
  { big, small :: f a
  , bigRnd, smallRnd :: [a]
  , bigSize :: Int
  , title :: String
  }

data BenchConfig f a = BenchConfig
  { fromList :: [a] -> f a
  , toList :: f a -> [a]
  , empty :: f a
  , singleton :: a -> f a
  , inserts, deletes :: [a] -> f a -> f a
  , notMembers, members :: [a] -> f a -> Bool
  , union, intersection, difference :: f a -> f a -> f a
  , mapc :: (a -> a) -> f a -> f a
  , confName :: String
  }

dietCfg :: DI.DietC a => BenchConfig DI.Set a
dietCfg = BenchConfig
  { fromList     = DI.fromList
  , toList       = DI.toList
  , empty        = DI.empty
  , singleton    = DI.singleton
  , inserts      = insertDI
  , deletes      = deleteDI
  , notMembers   = notMemberDI
  , members      = memberDI
  , union        = DI.union
  , intersection = DI.intersection
  , difference   = DI.difference
  , mapc         = DI.map
  , confName     = "Data.Diet"
  }

setCfg :: Ord a => BenchConfig ST.Set a
setCfg = BenchConfig
  { fromList     = ST.fromList
  , toList       = ST.toList
  , empty        = ST.empty
  , singleton    = ST.singleton
  , inserts      = insertST
  , deletes      = deleteST
  , notMembers   = notMemberST
  , members      = memberST
  , union        = ST.union
  , intersection = ST.intersection
  , difference   = ST.difference
  , mapc         = ST.map
  , confName     = "Data.Set"
  }

mkBench :: BenchConfig f Int -> BenchData f Int -> Benchmark
mkBench BenchConfig{..} BenchData{..} =
  bgroup (confName ++ "/" ++ title)
  [ bench "fromList"     (whnf fromList bigRnd)
  , bench "toList"       (whnf toList big)
  , bench "singleton"    (whnf singleton bigSize)
    -- Weak spot of Diet, because of lack of rebalancing
    --, bench "insert asc"   (whnf (inserts big) Set.empty)
  , bench "insert rnd"   (whnf (inserts bigRnd) empty)

    -- Weak spot of Diet in solid input case - to many split should be performed
    -- I suppose
    -- , bench "delete"       (whnf (deletes bigRnd) big)
  , bench "notMember"    (whnf (notMembers bigRnd) big)
  , bench "member"       (whnf (members bigRnd) big)
  , bench "intersection" (whnf (intersection small) big)
  , bench "difference"   (whnf (difference small) big)
    -- Weak spot pf Diet, because of lack of cool hedge-union algorithm
    --, bench "union"        (whnf (union small) big)
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

  let r   = mkStdGen 42
      bdSparse BenchConfig{..} = BenchData
        { small    = fromList eSparseSmall
        , big      = fromList eSparseBig
        , smallRnd = shuffle' eSparseBig n r
        , bigRnd   = shuffle' eSparseSmall (n `div` 2) r
        , title    = "Sparse input"
        , bigSize  = n
        }

      bdSolid BenchConfig{..} = BenchData
        { small    = fromList eSolidSmall
        , big      = fromList eSolidBig
        , smallRnd = shuffle' eSolidBig n r
        , bigRnd   = shuffle' eSolidSmall (n `div` 2) r
        , title    = "Solid input"
        , bigSize  = n
        }

  let dietSolid  = bdSolid dietCfg
      dietSparce = bdSparse dietCfg
      setSolid   = bdSolid setCfg
      setSparce  = bdSparse setCfg

  return $ rnf [B dietSolid, B dietSparce, B setSparce, B setSolid]

  defaultMain
    [ mkBench setCfg setSparce
    , mkBench dietCfg dietSparce
    , mkBench setCfg setSolid
    , mkBench dietCfg dietSolid
    ]

memberST :: Ord a => [a] -> ST.Set a -> Bool
memberST xs s = all (\x -> ST.member x s) xs

memberDI :: DI.DietC a => [a] -> DI.Set a -> Bool
memberDI xs bs = all (\x -> DI.member x bs) xs

notMemberST :: Ord a => [a] -> ST.Set a -> Bool
notMemberST xs s = all (\x -> ST.notMember x s) xs

notMemberDI :: DI.DietC a => [a] -> DI.Set a -> Bool
notMemberDI xs bs = all (\x -> DI.notMember x bs) xs

insertST :: Ord a => [a] -> ST.Set a -> ST.Set a
insertST xs s0 = foldl' (\s x -> ST.insert x s) s0 xs

insertDI :: DI.DietC a => [a] -> DI.Set a -> DI.Set a
insertDI xs bs0 = foldl' (\bs x -> DI.insert x bs) bs0 xs

deleteST :: Ord a => [a] -> ST.Set a -> ST.Set a
deleteST xs s0 = foldl' (\s x -> ST.delete x s) s0 xs

deleteDI :: DI.DietC a => [a] -> DI.Set a -> DI.Set a
deleteDI xs bs0 = foldl' (\bs x -> DI.delete x bs) bs0 xs
