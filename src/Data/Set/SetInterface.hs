{-# language
   ConstraintKinds
 #-}

module Data.Set.SetInterface(
   SetInterface(..)
 , dietInterface
 , setInterface
 , intSetInterface
 , bitSetInterface
 , wordBitSetInterface
) where

import qualified Data.Set as ST
import qualified Data.IntSet as IS
import qualified Data.BitSet as BS
import qualified Data.Set.Diet as DI
import qualified Data.Set.WordBitSet as WB

data SetInterface f a = SetInterface
  { fromList :: [a] -> f
  , toList :: f -> [a]
  , empty :: f
  , singleton :: a -> f
  , insert, delete :: a -> f -> f
  , notMember, member :: a -> f -> Bool
  , union, intersection, difference :: f -> f -> f
  , mapc :: (a -> a) -> f -> f
  , valid :: f -> Bool
  , findMin :: f -> a
  , confName :: String
  }

wordBitSetInterface :: SetInterface WB.Set Int
wordBitSetInterface = SetInterface
  { fromList     = WB.fromList
  , toList       = WB.toList
  , empty        = WB.empty
  , singleton    = WB.singleton
  , insert       = WB.insert
  , delete       = WB.delete
  , member       = WB.member
  , notMember    = WB.notMember
  , union        = WB.union
  , intersection = WB.intersection
  , difference   = WB.difference
  , mapc         = WB.map
  , valid        = const True
  , findMin      = WB.findMin
  , confName     = "Data.Set.WordBitSet"
  }

dietInterface :: DI.DietC a => SetInterface (DI.Set a) a
dietInterface = SetInterface
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
  , valid        = DI.valid
  , findMin      = DI.findMin
  , confName     = "Data.Set.Diet"
  }

setInterface :: Ord a => SetInterface (ST.Set a) a
setInterface = SetInterface
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
  , valid        = const True
  , findMin      = ST.findMin
  , confName     = "Data.Set"
  }

intSetInterface :: SetInterface IS.IntSet Int
intSetInterface = SetInterface
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
  , valid        = const True
  , findMin      = IS.findMin
  , confName     = "Data.IntSet"
  }

bitSetInterface :: SetInterface (BS.BitSet Int) Int
bitSetInterface = SetInterface
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
  , valid        = const True
  , findMin      = error "bitSetInterface: findMin isn't implemented."
  , confName     = "Data.BitSet"
  }
