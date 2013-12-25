data-set
========

A few more implementations of set ADT.

Particularly
* Data.Set.Diet which provides Discrete Interval Encoding Tree implementation.
  Look at http://web.engr.oregonstate.edu/~erwig/diet/ for reference.
* Data.Set.WordBitSet which is very similar to Data.IntSet, but it's aimed to represent set of integers within the range [0..n].
  Worst-case memory for WordBitSet is O(n).
