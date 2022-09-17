module SmallB where

import Data.Group
import Data.Matrix
import TwoElSet (grTable)

-- Constructing small groups exercise 4
data G4 = E | A | B | C deriving (Eq,Show)

instance Semigroup G4 where
   E <> x = x
   x <> E = x
   A <> B = C   -- cannot be A or B, because A or B would be E
   A <> C = B   
   B <> A = C
   B <> C = A
   C <> A = B
   C <> B = A
   _ <> _ = E   -- this is the diagonal in the table

instance Monoid G4 where
   mempty = E

instance Group G4 where
   invert x = x

tableG4 :: [G4] -> Matrix G4
tableG4 els = fromList 4  4 [ x <> y | x <- els, y <- els]

{- *SmallB> tableG4 [E,A,B,C]
┌         ┐
│ E A B C │
│ A E C B │
│ B C E A │
│ C B A E │
└         ┘
The group of subsets of a two-element set should be the same.
So, every operation on two different elements should yield the third,
and and an operation on itself should yield the neutral element.

From the Haskell program, adapted from Chapter 3 C,
for two elements, in file TwoElSet.hs:

*SmallB> grTable
┌                     ┐
│   ""  "a" "ab"  "b" │
│  "a"   ""  "b" "ab" │
│ "ab"  "b"   ""  "a" │
│  "b" "ab"  "a"   "" │
└                     ┘

The two tables are identical, when elements are renamed accordingly.
(A <=> "a", B <=> "ab", C <=> "b"

-}
