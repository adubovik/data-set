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
import qualified Data.IntSet as IS
import qualified Data.Diet as DI
import qualified Data.BitSet as BS

data B = forall a. NFData a => B a

instance NFData B where
    rnf (B b) = rnf b

instance (NFData f, NFData a) => NFData (BenchData f a) where
  rnf (BenchData {..}) = rnf [big, small] `seq`
                         rnf [bigRnd, smallRnd] `seq`
                         rnf bigSize `seq`
                         rnf title

data BenchData f a = BenchData
  { big, small :: f
  , bigRnd, smallRnd :: [a]
  , bigSize :: Int
  , title :: String
  }

data BenchConfig f a = BenchConfig
  { fromList :: [a] -> f
  , toList :: f -> [a]
  , empty :: f
  , singleton :: a -> f
  -- , inserts, deletes :: [a] -> f -> f
  , insert, delete :: a -> f -> f
  , notMember, member :: a -> f -> Bool
  -- , notMembers, members :: [a] -> f -> Bool
  , union, intersection, difference :: f -> f -> f
  , mapc :: (a -> a) -> f -> f
  , confName :: String
  }

dietCfg :: DI.DietC a => BenchConfig (DI.Set a) a
dietCfg = BenchConfig
  { fromList     = DI.fromList
  , toList       = DI.toList
  , empty        = DI.empty
  , singleton    = DI.singleton
  , insert       = DI.insert
  , delete       = DI.delete
  , member       = DI.member
  , notMember    = DI.notMember
  , union        = DI.union
  , intersection = DI.intersection
  , difference   = DI.difference
  , mapc         = DI.map
  , confName     = "Data.Diet"
  }

setCfg :: Ord a => BenchConfig (ST.Set a) a
setCfg = BenchConfig
  { fromList     = ST.fromList
  , toList       = ST.toList
  , empty        = ST.empty
  , singleton    = ST.singleton
  , insert       = ST.insert
  , delete       = ST.delete
  , member       = ST.member
  , notMember    = ST.notMember
  , union        = ST.union
  , intersection = ST.intersection
  , difference   = ST.difference
  , mapc         = ST.map
  , confName     = "Data.Set"
  }

intsetCfg :: BenchConfig IS.IntSet Int
intsetCfg = BenchConfig
  { fromList     = IS.fromList
  , toList       = IS.toList
  , empty        = IS.empty
  , singleton    = IS.singleton
  , insert       = IS.insert
  , delete       = IS.delete
  , member       = IS.member
  , notMember    = IS.notMember
  , union        = IS.union
  , intersection = IS.intersection
  , difference   = IS.difference
  , mapc         = IS.map
  , confName     = "Data.IntSet"
  }

bitsetCfg :: BenchConfig (BS.BitSet Int) Int
bitsetCfg = BenchConfig
  { fromList     = BS.fromList
  , toList       = BS.toList
  , empty        = BS.empty
  , singleton    = BS.singleton
  , insert       = BS.insert
  , delete       = BS.delete
  , member       = BS.member
  , notMember    = BS.notMember
  , union        = BS.union
  , intersection = BS.intersection
  , difference   = BS.difference
  , mapc         = BS.map
  , confName     = "Data.BitSet"
  }


mkBench :: BenchConfig f Int -> BenchData f Int -> Benchmark
mkBench b@BenchConfig{..} BenchData{..} =
  bgroup (confName ++ "/" ++ title)
  [ bench "fromList"     (whnf fromList bigRnd)
  , bench "toList"       (whnf toList big)
  , bench "singleton"    (whnf singleton bigSize)
    -- Weak spot of Diet, because of lack of rebalancing
    --, bench "insert asc"   (whnf (inserts big) Set.empty)
  , bench "insert rnd"   (whnf (inserts b bigRnd) empty)

    -- Weak spot of Diet in solid input case - to many split should be performed
    -- I suppose
    -- , bench "delete"       (whnf (deletes b bigRnd) big)
  , bench "notMember"    (whnf (notMembers b bigRnd) big)
  , bench "member"       (whnf (members b bigRnd) big)
  , bench "intersection" (whnf (intersection small) big)
  , bench "difference"   (whnf (difference small) big)
    -- Weak spot of Diet, because of lack of cool hedge-union algorithm
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
        , title    = "Sparse_input"
        , bigSize  = n
        }

      bdSolid BenchConfig{..} = BenchData
        { small    = fromList eSolidSmall
        , big      = fromList eSolidBig
        , smallRnd = shuffle' eSolidBig n r
        , bigRnd   = shuffle' eSolidSmall (n `div` 2) r
        , title    = "Solid_input"
        , bigSize  = n
        }

  let dietSolid  = bdSolid dietCfg
      dietSparse = bdSparse dietCfg
      setSolid   = bdSolid setCfg
      setSparse  = bdSparse setCfg
      intsetSolid = bdSolid intsetCfg
      intsetSparse = bdSparse intsetCfg
      bitsetSolid = bdSolid bitsetCfg
      bitsetSparse = bdSparse bitsetCfg


  return $ rnf [B dietSolid, B dietSparse,
                B setSolid, B setSparse,
                B intsetSolid, B intsetSparse,
                B bitsetSolid, B bitsetSparse]

  defaultMain
    [ mkBench setCfg setSparse
    , mkBench intsetCfg intsetSparse
    , mkBench bitsetCfg bitsetSparse
    , mkBench dietCfg dietSparse

    --, mkBench setCfg setSolid
    --, mkBench intsetCfg intsetSolid
    , mkBench bitsetCfg bitsetSolid
    , mkBench dietCfg dietSolid
    ]

members :: DI.DietC a => BenchConfig f a -> [a] -> f -> Bool
members BenchConfig{..} xs bs = all (\x -> member x bs) xs

notMembers :: DI.DietC a => BenchConfig f a -> [a] -> f -> Bool
notMembers BenchConfig{..} xs bs = all (\x -> notMember x bs) xs

inserts :: DI.DietC a => BenchConfig f a -> [a] -> f -> f
inserts BenchConfig{..} xs bs0 = foldl' (\bs x -> insert x bs) bs0 xs

deletes :: DI.DietC a => BenchConfig f a -> [a] -> f -> f
deletes BenchConfig{..} xs bs0 = foldl' (\bs x -> delete x bs) bs0 xs
