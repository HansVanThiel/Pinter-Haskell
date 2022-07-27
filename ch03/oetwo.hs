module Oetwo where

import Data.Group
import Data.Matrix
import Data.Either (fromRight)

-- elements of a finite group with Er for mistakes at at position 0
data G6 = Er | I | A | B | C | D | K 
                  deriving (Show, Eq, Bounded , Enum )

-- determines the 2 x 2 matrices
ma_I,ma_A,ma_B,ma_C,ma_D,ma_K :: Matrix Double -- without this, defaults to Integer!
ma_I = identity 2                              -- also Fractional class for inverse
ma_A = fromList 2 2 [0,1,1,0]
ma_B = fromList 2 2 [0,1,-1,-1]
ma_C = fromList 2 2 [ -1,-1,0,1]
ma_D = fromList 2 2 [ -1,-1,1,0]
ma_K = fromList 2 2 [1,0,-1,-1]
ma_Er = zero 2 2  -- default if matrix inverse fails (see later)

-- isomorphisms between G6 and the 2x2-matrices
gtoM :: G6 -> (Matrix Double)
gtoM g = case g of
         I -> ma_I
         A -> ma_A
         B -> ma_B
         C -> ma_C
         D -> ma_D
         K -> ma_K
         Er -> ma_Er -- fall back for mistakes

mtoG :: (Matrix Double) -> G6
mtoG m | m == ma_I = I  -- case does not work for matrices?!
       | m == ma_A = A
       | m == ma_B = B
       | m == ma_C = C
       | m == ma_D = D
       | m == ma_K = K
       | otherwise = Er -- fall back if matrix does not match
------------------------ end isomorphism definition
-- group definitions
instance Semigroup G6 where -- uses isomorphism
    g1 <> g2 = mtoG ((gtoM g1) `multStd` (gtoM g2))
instance Monoid G6 where
    mempty = I
instance Group G6 where 
    invert g = mtoG $ fromRight ma_Er (inverse (gtoM g))
------------------------- end group definitions
tableG6 :: Matrix G6 -- utilizes that numbers start at 1 (not 0)
tableG6 = matrix 6 6 (\(i,j) -> (toEnum i) <> (toEnum j))
{-
*Oetwo> tableG6
┌             ┐
│ I A B C D K │
│ A I C B K D │
│ B K D A I C │
│ C D K I A B │
│ D C I K B A │
│ K B A D C I │
└             ┘
-}

