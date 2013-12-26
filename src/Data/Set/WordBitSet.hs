{-# language
   MultiWayIf
 , NoMonomorphismRestriction
 , BangPatterns
 #-}

module Data.Set.WordBitSet(
   WordBitSet
 , Set

 , member
 , notMember

 , empty
 , null
 , singleton
 , insert
 , delete

 , union
 , difference
 , intersection

 , map

 , foldl'

 , toList
 , fromList

 , findMin
) where

import Prelude hiding (foldl, map, span, null)
import Data.Word(Word64)
import Data.Bits
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Control.DeepSeq (NFData(rnf))

data WordBitSet = WordBitSet !Int !WSet
  deriving (Eq, Ord, Show)

data WSet = Bin !WSet !WSet
          | Tip !Carry
          | Const !Bool
  deriving (Eq, Ord, Show)

type Set = WordBitSet
type Carry = Word64

instance NFData WordBitSet where
  rnf (WordBitSet c ws) = rnf c `seq` rnf ws

instance NFData WSet where
  rnf (Bin l r) = rnf l `seq` rnf r
  rnf (Tip   c) = rnf c
  rnf (Const b) = rnf b

-- Environment settings --

zero, ones :: Carry
zero = 0
ones = complement zero
{-# INLINE ones #-}

nbits :: Int
nbits = bitSize (0 :: Carry)
{-# INLINE nbits #-}

constToCarry :: Bool -> Carry
constToCarry True  = ones
constToCarry False = zero
{-# INLINE constToCarry #-}

-- Bit manipulation --

-- TODO: optimize via bit operations!
getPathPair :: Int -> (Int, Int)
getPathPair = (`quotRem` nbits)
{-# INLINE getPathPair #-}

getTreeIdx :: Int -> Int
getTreeIdx = (`quot` nbits)
{-# INLINE getTreeIdx #-}

_getWordIdx :: Int -> Int
_getWordIdx = (`rem` nbits)
{-# INLINE _getWordIdx #-}

d2, u2 :: Bits a => a -> a
d2 = (`shiftR` 1)
u2 = (`shiftL` 1)
{-# INLINE u2 #-}
{-# INLINE d2 #-}

foldlBits :: (Num i, Bits b) =>
             i -> (a -> i -> a) -> a -> b -> a
foldlBits !idx f !acc !bits = snd $ List.foldl' (\(!ix, !a) !bi ->
                                     if testBit bits bi
                                     then (ix+1, f a ix)
                                     else (ix+1,   a))
                              (idx, acc) [0..nbits-1]
{-# INLINE foldlBits #-}

_toListBits :: (Bits a, Num b) => b -> a -> [b]
_toListBits idx = List.reverse . foldlBits idx (flip (:)) []

-- Utilities --

tip :: Carry -> WSet
tip x | x == zero = Const False
      | x == ones = Const True
      | otherwise = Tip x
{-# INLINE tip #-}

bin :: WSet -> WSet -> WSet
bin (Const x) (Const y) | x == y = Const x
bin l r = Bin l r
{-# INLINE bin #-}

constToTip :: Bool -> WSet
constToTip True  = Tip ones
constToTip False = Tip zero
{-# INLINE constToTip #-}

increaseUpTo :: Int -> Set -> Set
increaseUpTo e (WordBitSet c ws) = go c ws
  where
    go curr wset
      | curr <= goal = go (u2 curr) (bin wset (Const False))
      | otherwise    = WordBitSet curr wset

    goal = getTreeIdx e
{-# INLINE increaseUpTo #-}

increaseUpToSize :: Int -> Set -> Set
increaseUpToSize c' (WordBitSet c ws) = go c ws
  where
    go curr wset
      | curr < c' = go (u2 curr) (bin wset (Const False))
      | otherwise = WordBitSet curr wset
{-# INLINE increaseUpToSize #-}

decrease :: Set -> Set
decrease (WordBitSet ca ws) = go ca ws
  where
    -- Could only shrink in size toward zero
    go i (Bin l (Const False)) = go (d2 i) l
    go _ (Const False) = empty
    go i wset = WordBitSet i wset
{-# INLINE decrease #-}

-- Meat --

empty :: Set
empty = WordBitSet 1 (Const False)

null :: Set -> Bool
null x = x==empty
{-# INLINE null #-}

singleton :: Int -> Set
singleton x = insert x empty
{-# INLINE singleton #-}

-- TODO: get rid of it
intro :: Int -> Set -> a -> (Int -> Int -> WSet -> a) -> a
intro i (WordBitSet c ws) def f
  | ti >= c   = def
  | otherwise = f wi ti ws
    where
      (ti, wi) = getPathPair i
{-# INLINE intro #-}

-- TODO: simplify
member :: Int -> Set -> Bool
member i s@(WordBitSet capacity _) = intro i s False wrap
  where
    wrap wordIdx treeIdx = go initBitIdx
      where
        -- TODO: something with this
        initBitIdx = {-# SCC "member.popCount" #-} popCount (capacity - 1)

        go bitIdx (Bin l r)
          | testBit treeIdx bitIdx' = go bitIdx' r
          | otherwise               = go bitIdx' l
          where
            bitIdx' = bitIdx - 1
        go _{-0-} (Tip w) = testBit w wordIdx
        go _ (Const c)    = c

notMember :: Int -> Set -> Bool
notMember i = not . (member i)
{-# INLINE notMember #-}

set :: Bool -> Int -> Set -> Set
set val idx wbs@(WordBitSet capacity _)
  | treeIdx >= capacity
  , val == False
  = wbs

  | otherwise = WordBitSet capacity' (set' initBitIdx ws')

  where
    WordBitSet capacity' ws' = increaseUpTo idx wbs

    (treeIdx, wordIdx) = getPathPair idx
    initBitIdx = {-# SCC "set.popCount" #-} popCount (capacity' - 1)

    set' bitIdx (Bin l r)
      | testBit treeIdx bitIdx' = bin l (set' bitIdx' r)
      | otherwise               = bin   (set' bitIdx' l) r
      where
        bitIdx' = bitIdx - 1
    set' bitIdx s@(Const c)
      | bitIdx == 0 = set' bitIdx (constToTip c)
      | val         = set' bitIdx (Bin s s)
      | otherwise   = s
    set' _{-0-}  (Tip w)
      | val       = tip (  setBit w wordIdx)
      | otherwise = tip (clearBit w wordIdx)

delete :: Int -> Set -> Set
delete i = decrease . set False i
{-# INLINE delete #-}

insert :: Int -> Set -> Set
insert i = set True i
{-# INLINE insert #-}

foldl' :: Num i => (a -> i -> a) -> a -> Set -> a
foldl' f !initAcc (WordBitSet capacity wset) =
  go 0 capacity wset initAcc
  where
    nb = fromIntegral nbits

    go !start !span !ws !acc = case ws of
      Bin l r -> go start' span' r $ go start span' l acc
      Const v -> if | span /= 1 -> go start' span' (Const v) $
                                     go start span' (Const v) acc
                    | v == True -> foldlBits (nb*start) f acc ones
                    | otherwise -> acc
      Tip w   -> foldlBits (nb*start) f acc w
      where
        span'  = d2 span
        start' = start + fromIntegral span'

fromList :: [Int] -> Set
fromList = Foldable.foldl' (flip insert) empty
{-# INLINE fromList #-}

-- TODO: Implement via foldr
toList :: Num a => Set -> [a]
toList = List.reverse . foldl' (flip (:)) []
{-# INLINE toList #-}

unionWith :: (Carry -> Carry -> Carry) -> Set -> Set -> Set
unionWith f wbs1@(WordBitSet c1 _) wbs2@(WordBitSet c2 _) =
  decrease $ unionWith' (increaseUpToSize c2 wbs1)
                        (increaseUpToSize c1 wbs2)
  where
    unionWith' (WordBitSet _ ws1) (WordBitSet _ ws2) =
      WordBitSet (max c1 c2) (go ws1 ws2)

    toCarry (Tip   c) = c
    toCarry (Const c) = constToCarry c
    toCarry x = error $ "unionWith: Impossible combination " ++ show x

    go (Bin l1 r1) (Bin l2 r2) = bin (go l1 l2) (go r1 r2)
    go (Bin l1 r1) c@(Const _) = bin (go l1  c) (go r1  c)
    go c@(Const _) (Bin l2 r2) = bin (go c  l2) (go c  r2)
    go l r                     = tip $ f (toCarry l) (toCarry r)

union :: Set -> Set -> Set
union = unionWith (.|.)
{-# INLINE union #-}

intersection :: Set -> Set -> Set
intersection = unionWith (.&.)
{-# INLINE intersection #-}

difference :: Set -> Set -> Set
difference = unionWith (\x y -> x .&. (complement y))
{-# INLINE difference #-}

map :: (Int -> Int) -> Set -> Set
map f = fromList . List.map f . toList
{-# INLINE map #-}

findMin :: Set -> Int
findMin wbs@(WordBitSet capacity ws) = go 0 capacity ws
  where
    go start span (Bin l r) =
      case l of
        Const False -> go start' span' r
        _           -> go start  span' l
      where
        span'  = d2 span
        start' = start + span'

    go  start _span (Const True ) = nbits * start
    go _start _span (Const False) = error $ "findMin: empty tree: " ++ show wbs

    go start _span (Tip w)
      | w /= 0 = nbits * start + firstBit w
      | otherwise = error $ "findMin: inconsistent tree: " ++ show wbs

    firstBit w = popCount (w `xor` (w - 1)) - 1
