module SmallA where

import Data.Group
import Data.Matrix

-- F Constructing Small Groups

-- exercise 3
data G3 = E | A | B deriving (Show)

instance Semigroup G3 where
    E <> x = x
    x <> E = x
    A <> A = B  -- only other possibilities A or E, but see below
    A <> B = E
    B <> B = A
    B <> A = E

{- suppose A <> A = A  
 -         A <> E = A
 - then    A = E
 -
 - suppose  A <> A = E then 
 -          A <> B must be B but
 -          E <> B = B
 - then     A = E
 -}
        
instance Monoid G3 where
    mempty = E

instance Group G3 where
    invert E = E
    invert A = B
    invert B = A

-- construct group table from list of the three elements
tableG3 :: Group a => [a] -> Matrix a
tableG3 els = fromList 3 3 [x <> y | x <- els, y <- els]


-- addition modulo 3 is another group with three elements 
newtype Z3 = Z3 {getInt :: Int} deriving (Show)

instance Semigroup Z3 where
    (Z3 x) <> (Z3 y) = Z3 $ ((x `mod`3) + (y `mod` 3 )) `mod` 3
instance Monoid Z3 where
     mempty = Z3 0
instance Group Z3 where
     invert (Z3 x) | x `mod` 3 == 0 = Z3 0
                   | otherwise = Z3 (3 - (x `mod` 3))

{- 
*SmallA> tableG3 [Z3 0,Z3 1,Z3 2]
┌                                                 ┐
│ Z3 {getInt = 0} Z3 {getInt = 1} Z3 {getInt = 2} │
│ Z3 {getInt = 1} Z3 {getInt = 2} Z3 {getInt = 0} │
│ Z3 {getInt = 2} Z3 {getInt = 0} Z3 {getInt = 1} │
└                                                 ┘
*SmallA> fmap getInt $ tableG3 [Z3 0,Z3 1,Z3 2]
┌       ┐
│ 0 1 2 │
│ 1 2 0 │
│ 2 0 1 │
└       ┘
*SmallA> tableG3 [E,A,B]
┌       ┐
│ E A B │
│ A B E │
│ B E A │
└       ┘
 -}
-- every three element group has the same structure
