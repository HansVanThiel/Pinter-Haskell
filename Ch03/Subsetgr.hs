module Subsetgr where

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
threeS :: Set Char
threeS = Set.fromList "abc" 

-- set of subsets
subS :: Set (Set Char)
subS = Set.powerSet threeS

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
{- *Subsetgr> grTable
 -

┌                                                 ┐
│    ""   "a"  "ab" "abc"  "ac"   "b"  "bc"   "c" │
│   "a"    ""   "b"  "bc"   "c"  "ab" "abc"  "ac" │
│  "ab"   "b"    ""   "c"  "bc"   "a"  "ac" "abc" │
│ "abc"  "bc"   "c"    ""   "b"  "ac"   "a"  "ab" │
│  "ac"   "c"  "bc"   "b"    "" "abc"  "ab"   "a" │
│   "b"  "ab"   "a"  "ac" "abc"    ""   "c"  "bc" │
│  "bc" "abc"  "ac"   "a"  "ab"   "c"    ""   "b" │
│   "c"  "ac" "abc"  "ab"   "a"  "bc"   "b"    "" │
└                                                 ┘
-}
