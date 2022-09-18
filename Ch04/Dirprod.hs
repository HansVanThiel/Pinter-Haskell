module Dirprod where

import Data.Group
import Data.Matrix

newtype Z2 = Z2 Int deriving (Show)

instance Semigroup Z2 where
    (Z2 x) <> (Z2 y) = Z2 $ ((x `mod`2) + (y `mod` 2 )) `mod` 2
instance Monoid Z2 where
     mempty = Z2 0
instance Group Z2 where
     invert (Z2 x) | x `mod` 2 == 0 = Z2 0
                   | otherwise = Z2 (2 - (x `mod` 2))


newtype Z3 = Z3 Int deriving (Show)

instance Semigroup Z3 where
    (Z3 x) <> (Z3 y) = Z3 $ ((x `mod`3) + (y `mod` 3 )) `mod` 3
instance Monoid Z3 where
     mempty = Z3 0
instance Group Z3 where
     invert (Z3 x) | x `mod` 3 == 0 = Z3 0
                   | otherwise = Z3 (3 - (x `mod` 3))

{- *Dirprod> mempty::(Z2,Z3)
(Z2 0,Z3 0)
*Dirprod> (Z2 1,Z3 2) <> (Z2 0,Z3 1)
(Z2 1,Z3 0)
*Dirprod> (Z2 1,Z3 1) <> (Z2 1,Z3 1)
(Z2 0,Z3 2)
*Dirprod> pow (Z2 1, Z3 1) 5
(Z2 1,Z3 2)
*Dirprod> invert (Z2 1,Z3 2)
(Z2 1,Z3 1)
-}

prodels :: [(Z2,Z3)]
prodels = [ (x,y) | x <- [Z2 0,Z2 1], y <- [Z3 0, Z3 1, Z3 2] ]

tableG6 :: Matrix (Z2,Z3)
tableG6 = fromList 6 6 [x <> y | x <- prodels, y <- prodels ]

{-
*Dirprod> tableG6
┌                                                                         ┐
│ (Z2 0,Z3 0) (Z2 0,Z3 1) (Z2 0,Z3 2) (Z2 1,Z3 0) (Z2 1,Z3 1) (Z2 1,Z3 2) │
│ (Z2 0,Z3 1) (Z2 0,Z3 2) (Z2 0,Z3 0) (Z2 1,Z3 1) (Z2 1,Z3 2) (Z2 1,Z3 0) │
│ (Z2 0,Z3 2) (Z2 0,Z3 0) (Z2 0,Z3 1) (Z2 1,Z3 2) (Z2 1,Z3 0) (Z2 1,Z3 1) │
│ (Z2 1,Z3 0) (Z2 1,Z3 1) (Z2 1,Z3 2) (Z2 0,Z3 0) (Z2 0,Z3 1) (Z2 0,Z3 2) │
│ (Z2 1,Z3 1) (Z2 1,Z3 2) (Z2 1,Z3 0) (Z2 0,Z3 1) (Z2 0,Z3 2) (Z2 0,Z3 0) │
│ (Z2 1,Z3 2) (Z2 1,Z3 0) (Z2 1,Z3 1) (Z2 0,Z3 2) (Z2 0,Z3 0) (Z2 0,Z3 1) │
└                                                                         ┘
 -}
 
