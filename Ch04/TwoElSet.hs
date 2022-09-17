module TwoElSet where

-- modified frpm Subsetgr.hs in Ch03 

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Group
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat

-- <> is already taken by set, so define a newtype
newtype Aset a = Aset {getSet :: Set a} 
    deriving (Show, Eq, Ord)

asetAdd :: Ord a => Aset a -> Aset a -> Aset a
asetAdd (Aset u) (Aset v) = 
    Aset $ Set.difference (Set.union u v) (Set.intersection u v)

-- asetAdd is associative (and commutative)
instance Ord a => Semigroup (Aset a) where
        (<>) = asetAdd
instance Ord a => Monoid (Aset a) where
        mempty = Aset (Set.empty)
instance Ord a => Group (Aset a) where
        invert x = x
instance Ord a => Abelian (Aset a)
-----------------------------------------------------
-- elements
twoS :: Set Char
twoS = Set.fromList "ab" 

-- set of subsets
subS :: Set (Set Char)
subS = Set.powerSet twoS

-- list of subsets
elemLs :: [Set Char]
elemLs = Set.toList subS

-- convert to group type
asetLs :: [Aset Char]
asetLs = map Aset elemLs

-- calculate group
elemGr :: [Aset Char]
elemGr = [ x <> y | x <- asetLs, y <- asetLs ]

-- convert to list
back2Ls :: [String]
back2Ls = map (Set.toList . getSet) elemGr

-- display in matrix
grTable :: Matrix String
grTable = Mat.fromList (Set.size subS) (Set.size subS) back2Ls

{- *TwoElSet> grTable
┌                     ┐
│   ""  "a" "ab"  "b" │
│  "a"   ""  "b" "ab" │
│ "ab"  "b"   ""  "a" │
│  "b" "ab"  "a"   "" │
└                     ┘
-}

