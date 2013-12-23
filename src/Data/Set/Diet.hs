{-# language
   ConstraintKinds
 , MultiWayIf
 , StandaloneDeriving
 , DeriveDataTypeable
 , BangPatterns
 , TypeSynonymInstances
 , FlexibleInstances
 , UndecidableInstances
 #-}

-- Should turn UndecidableInstances on, because of some
-- constraint kind related bug

module Data.Set.Diet (
 -- * Diet type
   Diet       -- instance Eq,Ord,Show,Read,NFData
 , Set        -- type synonym of Diet

 -- * Diet constraint
 , DietC

 -- * Operators
 , (\\)

 -- * Query
 , null
 , size
 , member
 , notMember
 -- , lookupLT
 -- , lookupGT
 -- , lookupLE
 -- , lookupGE
 -- , isSubsetOf
 -- , isProperSubsetOf

 -- * Construction
 , empty
 , singleton, singletonInterval
 , insert, insertInterval
 , delete, deleteInterval

 -- * Combine
 , union
 , unions
 , difference
 , intersection

 -- -- * Filter
 -- , filter
 -- , partition
 -- , split
 -- , splitMember

 -- -- * Indexed
 -- , lookupIndex
 -- , findIndex
 -- , elemAt
 -- , deleteAt

 -- * Map
 , map, mapInterval
 , mapMonotonic, mapMonotonicInterval

 -- * Folds
 , foldr, foldrInterval
 , foldl, foldlInterval
 -- ** Strict folds
 , foldr', foldrInterval'
 , foldl', foldlInterval'
 -- -- ** Legacy folds
 -- , fold

 -- * min\/Max
 , findMin, findMinInterval
 , findMax, findMaxInterval
 , deleteMin, deleteMinInterval
 , deleteMax, deleteMaxInterval
 , deleteFindMin, deleteFindMinInterval
 , deleteFindMax, deleteFindMaxInterval
 , maxView, maxViewInterval
 , minView, minViewInterval

 -- * Conversion

 -- ** List
 , elems, elemsInterval
 , toList, toListInterval
 , fromList, fromListInterval

 -- ** Ordered list
 , toAscList, toAscListInterval
 , toDescList, toDescListInterval
 -- , fromAscList
 -- , fromDistinctAscList

 -- ** Diet specific
 , mergeLeft, mergeRight

 -- * Debugging
 , showTree
 , showTreeWith
 , valid

 -- Internals (for testing)
 -- , bin
 -- , balanced
 -- , join
 -- , merge
 )
   where

import Prelude hiding (filter,foldl,foldr,null,map)
import qualified Data.List as List
import Data.Monoid (Monoid(..),(<>), Sum(..))
import Control.DeepSeq (NFData(rnf))

import Text.Read

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: DietC a => Diet a -> Diet a -> Diet a
m1 \\ m2 = difference m1 m2
{-# INLINABLE (\\) #-}

{--------------------------------------------------------------------
  Diets are interval sets
--------------------------------------------------------------------}
-- | A set of values @a@.

data Diet a   = Bin !(a,a) !(Diet a) !(Diet a)
              | Tip
type Set      = Diet
type DietC a  = (Enum a, Ord a)

instance DietC a => Monoid (Diet a) where
    mempty  = empty
    mappend = union
    mconcat = unions

-- Alas, can't make instance (Foldable.Foldable Diet) because of
-- Enum constraint. DatatypeContexts is depricated :(

foldMapInterval :: Monoid m => ((a,a) -> m) -> Diet a -> m
foldMapInterval f = go
  where go Tip = mempty
        go (Bin k l r) = go l <> (f k <> go r)
{-# INLINE foldMapInterval #-}

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is this the empty set?
null :: Diet a -> Bool
null Tip      = True
null (Bin {}) = False
{-# INLINE null #-}

-- | /O(n)/. The number of elements in the set.
size :: DietC a => Diet a -> Int
size = getSum . foldMapInterval isize
  where
    isize (x,y) = Sum $ fromEnum y - fromEnum x + 1
    {-# INLINE isize #-}
{-# INLINE size #-}

-- | /O(log n)/. Is the element in the set?
member :: DietC a => a -> Diet a -> Bool
member = go
  where
    go _ Tip = False
    go !a (Bin (x,y) l r) =
      if | a < x -> go a l
         | a <= y -> True
         | otherwise -> go a r
{-# INLINABLE member #-}

-- | /O(log n)/. Is the element not in the set?
notMember :: DietC a => a -> Diet a -> Bool
notMember a = not . member a
{-# INLINABLE notMember #-}


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty diet.
empty  :: Diet a
empty = Tip
{-# INLINE empty #-}

-- | /O(1)/. Create a singleton diet.
singleton :: a -> Diet a
singleton x = Bin (x,x) Tip Tip
{-# INLINE singleton #-}

-- | /O(1)/. Create a set with single interval.
singletonInterval :: (a,a) -> Diet a
singletonInterval i = Bin i Tip Tip
{-# INLINE singletonInterval #-}

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.

-- See Note: Type of local 'go' function
insert :: DietC a => a -> Diet a -> Diet a
insert a = {-# SCC "Diet.insert" #-} insertInterval (a,a)
{-# INLINABLE insert #-}

insertInterval :: DietC a => (a,a) -> Diet a -> Diet a
insertInterval = {-# SCC "Diet.insertI" #-} go
  where
    go :: DietC a => (a,a) -> Diet a -> Diet a
    go !i Tip = {-# SCC "Diet.insertI1" #-} singletonInterval i
    go !i@(x,y) d@(Bin i'@(x',y') l r)
      | succ y  < x' = {-# SCC "Diet.insertI2" #-} Bin i' (go i l) r
      | succ y' < x  = {-# SCC "Diet.insertI3" #-} Bin i' l (go i r)
      | x' <= x && y <= y' = d
      | otherwise = {-# SCC "Diet.insert.joins" #-} joinLeft . joinRight $ Bin im l' r'
        where
          im = (min x x', max y y')
          l' | x < x' = deleteInterval (x, pred x') l
             | otherwise = l
          r' | y' < y = deleteInterval (succ y', y) r
             | otherwise = r
{-# INLINABLE insertInterval #-}

splitMax :: Diet a -> Maybe ((a,a), Diet a)
splitMax (Bin i l r) = case splitMax r of
  Nothing       -> Just (i, l)
  Just (i', r') -> Just (i', Bin i l r')
splitMax Tip = Nothing

splitMin :: Diet a -> Maybe ((a,a), Diet a)
splitMin (Bin i l r) = case splitMin l of
  Nothing       -> Just (i, r)
  Just (i', l') -> Just (i', Bin i l' r)
splitMin Tip = Nothing

joinLeft :: DietC a => Diet a -> Diet a
joinLeft d@(Bin (x,y) l r) = {-# SCC "Diet.joinLeft" #-} case splitMax l of
  Nothing -> d
  Just ((x',y'), l') ->
    if | succ y' == x -> Bin (x',y) l' r
       | otherwise -> d
joinLeft Tip = Tip
{-# INLINABLE joinLeft #-}

joinRight :: DietC a => Diet a -> Diet a
joinRight d@(Bin (x,y) l r) = {-# SCC "Diet.joinRight" #-} case splitMin r of
  Nothing -> d
  Just ((x',y'), r') ->
    if | succ y == x' -> Bin (x,y') l r'
       | otherwise -> d
joinRight Tip = Tip
{-# INLINABLE joinRight #-}

mergeRight :: Diet a -> Diet a -> Diet a
mergeRight l r = case splitMin r of
  Nothing -> l
  Just (i, r') -> Bin i l r'
{-# INLINABLE mergeRight #-}

mergeLeft :: Diet a -> Diet a -> Diet a
mergeLeft l r = case splitMax l of
  Nothing -> r
  Just (i, l') -> Bin i l' r
{-# INLINABLE mergeLeft #-}

deleteInterval :: DietC a => (a,a) -> Diet a -> Diet a
deleteInterval !(x,y) (Bin i'@(x',y') l r)
  | x  <= x' && y' <= y  = {-# SCC "Diet.deleteI.1" #-} mergeRight l' r'
  | x' <  x  && y  <  y' = {-# SCC "Diet.deleteI.2" #-} Bin (succ y, y') (Bin (x', pred x) l Tip) r
  | otherwise = {-# SCC "Diet.deleteI.3" #-} Bin i'' l' r'
  where
    i''
      | y < x' || y' < x = i'
      | x  <= x' = (succ y, y')
      | y' <= y  = (x', pred x)
      | otherwise = error "Data.Diet.deleteInterval: Impossible!"
    r'
      | y' < y = deleteInterval (max (succ y') x, y) r
      | otherwise = r
    l'
      | x < x' = deleteInterval (x, min (pred x') y) l
      | otherwise = l
deleteInterval _ Tip = Tip

-- | /O(log n)/. Delete an element from a set.

-- See Note: Type of local 'go' function
delete :: DietC a => a -> Diet a -> Diet a
delete a = {-# SCC "Diet.delete" #-} deleteInterval (a,a)
{-# INLINABLE delete #-}

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
findMinInterval :: Diet a -> (a,a)
findMinInterval (Bin x Tip _) = x
findMinInterval (Bin _ l _)   = findMinInterval l
findMinInterval Tip           = error "Diet.findMinInterval: empty set has no minimal interval"

-- | /O(log n)/. The minimal element of a set.
findMin :: Diet a -> a
findMin = fst . findMinInterval
{-# INLINE findMin #-}

-- | /O(log n)/. The maximal element of a set.
findMaxInterval :: Diet a -> (a,a)
findMaxInterval (Bin x _ Tip)  = x
findMaxInterval (Bin _ _ r)    = findMaxInterval r
findMaxInterval Tip            = error "Diet.findMaxInterval: empty set has no maximal interval"

-- | /O(log n)/. The minimal element of a set.
findMax :: Diet a -> a
findMax = snd . findMaxInterval
{-# INLINE findMax #-}

-- | /O(log n)/. Delete the minimal element. Returns an empty set if the set is empty.
deleteMin :: DietC a => Diet a -> Diet a
deleteMin (Bin (x,y) Tip r)
  | x == y = r
  | otherwise = Bin (succ x, y) Tip r
deleteMin (Bin x l r)   = Bin x (deleteMin l) r
deleteMin Tip           = Tip

deleteMinInterval :: Diet a -> Diet a
deleteMinInterval (Bin _ Tip r) = r
deleteMinInterval (Bin x l r)   = Bin x (deleteMinInterval l) r
deleteMinInterval Tip           = Tip

-- | /O(log n)/. Delete the maximal element. Returns an empty set if the set is empty.
deleteMax :: DietC a => Diet a -> Diet a
deleteMax (Bin (x,y) l Tip)
  | x == y = l
  | otherwise = Bin (x, pred y) l Tip
deleteMax (Bin x l r)   = Bin x l (deleteMax r)
deleteMax Tip           = Tip

deleteMaxInterval :: Diet a -> Diet a
deleteMaxInterval (Bin _ l Tip) = l
deleteMaxInterval (Bin x l r)   = Bin x l (deleteMaxInterval r)
deleteMaxInterval Tip           = Tip

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: DietC a => [Diet a] -> Diet a
unions = foldlStrict union empty
{-# INLINABLE unions #-}

-- | /O(m * log(n))/. The union of two sets, preferring the first set when
-- equal elements are encountered.
union :: DietC a => Diet a -> Diet a -> Diet a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = foldlInterval' (flip insertInterval) t1 t2
{-# INLINABLE union #-}

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(m * log(n))/. Difference of two sets.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: DietC a => Diet a -> Diet a -> Diet a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = foldlInterval' (flip deleteInterval) t1 t2
{-# INLINABLE difference #-}


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O((n+m)*log(n+m))/. The intersection of two diets.  Elements of the
-- result come from the first diet, so for example
--
-- > import qualified Data.Diet as D
-- > data AB = A | B deriving Show
-- > instance Ord AB where compare _ _ = EQ
-- > instance Eq AB where _ == _ = True
-- > main = print (D.singleton A `D.intersection` D.singleton B,
-- >               D.singleton B `D.intersection` D.singleton A)
--
-- prints @(fromList [A],fromList [B])@.
intersection :: DietC a => Diet a -> Diet a -> Diet a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 t2 = fromListInterval $ toListInterval t1 `intersectLists` toListInterval t2
  where
    intersectLists :: DietC a => [(a,a)] -> [(a,a)] -> [(a,a)]
    intersectLists [] _ = []
    intersectLists _ [] = []
    intersectLists o@((x,y):r) o'@((x',y'):r')
      | y  < x' = intersectLists r o'
      | y' < x  = intersectLists o r'
      | y == y' = i : intersectLists r r'
      | y  < y' = i : intersectLists r ((succ y, y'):r')
      | True    = i : intersectLists ((succ y', y):r) r'
      where i = (max x x', min y y')
{-# INLINABLE intersection #-}

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*log n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: (Enum a, DietC b) => (a -> b) -> Diet a -> Diet b
map f = fromList . List.map f . toList
{-# INLINABLE map #-}

mapInterval :: DietC b => ((a,a) -> (b,b)) -> Diet a -> Diet b
mapInterval f = fromListInterval . List.map f . toListInterval
{-# INLINABLE mapInterval #-}

-- | /O(n)/. The
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s

mapMonotonic :: (a -> b) -> Diet a -> Diet b
mapMonotonic _ Tip = Tip
mapMonotonic f (Bin (x, y) l r) = Bin (f x, f y) (mapMonotonic f l) (mapMonotonic f r)

mapMonotonicInterval :: ((a,a) -> (b,b)) -> Diet a -> Diet b
mapMonotonicInterval _ Tip = Tip
mapMonotonicInterval f (Bin i l r) =
  Bin (f i) (mapMonotonicInterval f l) (mapMonotonicInterval f r)

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldrInterval :: ((a,a) -> b -> b) -> b -> Diet a -> b
foldrInterval f z = go z
  where
    go z' Tip         = z'
    go z' (Bin x l r) = go (f x (go z' r)) l
{-# INLINE foldrInterval #-}

foldr :: Enum a => (a -> b -> b) -> b -> Diet a -> b
foldr f = foldrInterval (\(x,y) b -> List.foldr f b (enumFromTo x y))
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrInterval' :: ((a,a) -> b -> b) -> b -> Diet a -> b
foldrInterval' f z = go z
  where
    go !z' Tip         = z'
    go !z' (Bin x l r) = go (f x (go z' r)) l
{-# INLINE foldrInterval' #-}

foldr' :: Enum a => (a -> b -> b) -> b -> Diet a -> b
foldr' f = foldrInterval' (\(x,y) b -> List.foldr f b (enumFromTo x y))
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldlInterval :: (a -> (b,b) -> a) -> a -> Diet b -> a
foldlInterval f z = go z
  where
    go z' Tip         = z'
    go z' (Bin x l r) = go (f (go z' l) x) r
{-# INLINE foldlInterval #-}

foldl :: Enum b => (a -> b -> a) -> a -> Diet b -> a
foldl f = foldlInterval (\a (x,y) -> List.foldl f a (enumFromTo x y))
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlInterval' :: (a -> (b,b) -> a) -> a -> Diet b -> a
foldlInterval' f z = go z
  where
    go !z' Tip         = z'
    go !z' (Bin x l r) = go (f (go z' l) x) r
{-# INLINE foldlInterval' #-}

foldl' :: Enum b => (a -> b -> a) -> a -> Diet b -> a
foldl' f = foldlInterval (\a (x,y) -> List.foldl' f a (enumFromTo x y))
{-# INLINE foldl' #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: Enum a => Diet a -> [a]
elems = toAscList

elemsInterval :: Diet a -> [(a,a)]
elemsInterval = toAscListInterval

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the set to a list of elements. Subject to list fusion.
toList :: Enum a => Diet a -> [a]
toList = toAscList

toListInterval :: Diet a -> [(a,a)]
toListInterval = toAscListInterval

-- | /O(n)/. Convert the set to an ascending list of elements. Subject to list fusion.
toAscList :: Enum a => Diet a -> [a]
toAscList = foldr (:) []

toAscListInterval :: Diet a -> [(a,a)]
toAscListInterval = foldrInterval (:) []

-- | /O(n)/. Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: Enum a => Diet a -> [a]
toDescList = foldl (flip (:)) []

toDescListInterval :: Diet a -> [(a,a)]
toDescListInterval = foldlInterval (flip (:)) []

-- | /O(n*log n)/. Create a set from a list of elements.
fromListInterval :: DietC a => [(a,a)] -> Diet a
fromListInterval = go . List.sort
  where
    go [] = Tip
    go [x] = singletonInterval x
    go xs = Bin m (fromListInterval l) (fromListInterval r)
      where
        (l,m:r) = splitAt ((length xs) `div` 2) xs
{-# INLINABLE fromListInterval #-}

fromList :: DietC a => [a] -> Diet a
fromList [] = Tip
fromList xs = fromListInterval .
              reverse . groupInterval .
              List.map head .
              List.group . List.sort $ xs
  where
    groupInterval ls = go (dup $ head ls) [] (tail ls)
    dup x = (x,x)

    go !i res [] = i:res
    go !(p,y) res (y':ls)
      | succ y == y' = go (p, y') res ls
      | otherwise    = go (y',y') ((p,y):res) ls
{-# INLINE fromList #-}


{--------------------------------------------------------------------
  Eq converts the set to a list. In a lazy setting, this
  actually seems one of the faster methods to compare two trees
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance DietC a => Eq (Diet a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance DietC a => Ord (Diet a) where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (DietC a, Show a) => Show (Diet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read a, Ord a, Enum a) => Read (Diet a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault

{--------------------------------------------------------------------
  NFData
--------------------------------------------------------------------}

instance NFData a => NFData (Diet a) where
    rnf Tip         = ()
    rnf (Bin y l r) = rnf y `seq` rnf l `seq` rnf r

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)

deleteFindMin :: DietC a => Diet a -> (a,Diet a)
deleteFindMin t
  = case t of
      Bin (x,y) Tip r ->
        if (succ x == y)
        then (x,r)
        else (x, Bin (succ x, y) Tip r)
      Bin x l r   -> let (xm,l') = deleteFindMin l in (xm, Bin x l' r)
      Tip         -> (error "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

deleteFindMinInterval :: Diet a -> ((a,a),Diet a)
deleteFindMinInterval t
  = case t of
      Bin x Tip r -> (x,r)
      Bin x l r   -> let (xm,l') = deleteFindMinInterval l in (xm, Bin x l' r)
      Tip         -> (error "Set.deleteFindMinInterval: can not return the minimal element of an empty set", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: DietC a => Diet a -> (a,Diet a)
deleteFindMax t
  = case t of
      Bin (x,y) l Tip ->
        if (succ x == y)
        then (x,l)
        else (y, Bin (x, pred y) l Tip)
      Bin x l r   -> let (xm,r') = deleteFindMax r in (xm, Bin x l r')
      Tip         -> (error "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

deleteFindMaxInterval :: Diet a -> ((a,a),Diet a)
deleteFindMaxInterval t
  = case t of
      Bin x l Tip -> (x,l)
      Bin x l r   -> let (xm,r') = deleteFindMaxInterval r in (xm, Bin x l r')
      Tip         -> (error "Set.deleteFindMaxInterval: can not return the maximal element of an empty set", Tip)

-- | /O(log n)/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: DietC a => Diet a -> Maybe (a, Diet a)
minView Tip = Nothing
minView x = Just (deleteFindMin x)

minViewInterval :: Diet a -> Maybe ((a,a), Diet a)
minViewInterval Tip = Nothing
minViewInterval x = Just (deleteFindMinInterval x)

-- | /O(log n)/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: DietC a => Diet a -> Maybe (a, Diet a)
maxView Tip = Nothing
maxView x = Just (deleteFindMax x)

maxViewInterval :: Diet a -> Maybe ((a,a), Diet a)
maxViewInterval Tip = Nothing
maxViewInterval x = Just (deleteFindMaxInterval x)

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs
{-# INLINE foldlStrict #-}

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => Diet a -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
 the tree that implements the set. If @hang@ is
 @True@, a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

> Set> putStrLn $ showTreeWith True False $ fromDistinctAscList [1..5]
> 4
> +--2
> |  +--1
> |  +--3
> +--5
>
> Set> putStrLn $ showTreeWith True True $ fromDistinctAscList [1..5]
> 4
> |
> +--2
> |  |
> |  +--1
> |  |
> |  +--3
> |
> +--5
>
> Set> putStrLn $ showTreeWith False True $ fromDistinctAscList [1..5]
> +--5
> |
> 4
> |
> |  +--3
> |  |
> +--2
>    |
>    +--1

-}
showTreeWith :: Show a => Bool -> Bool -> Diet a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> Diet a -> ShowS
showsTree wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin x Tip Tip
          -> showsBars lbars . shows x . showString "\n"
      Bin x l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . shows x . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: Show a => Bool -> [String] -> Diet a -> ShowS
showsTreeHang wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n"
      Bin x Tip Tip
          -> showsBars bars . shows x . showString "\n"
      Bin x l r
          -> showsBars bars . shows x . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars


{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal set structure is valid.
valid :: DietC a => Diet a -> Bool
valid t = ordered t

ordered :: DietC a => Diet a -> Bool
ordered t
  = bounded (const True) (const True) t
  where
    bounded lo hi t'
      = case t' of
          Tip           -> True
          Bin (x,y) l r -> lo x &&
                           hi y &&
                           bounded lo (< pred x) l &&
                           bounded (> succ x) hi r
