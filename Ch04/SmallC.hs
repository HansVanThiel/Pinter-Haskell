module SmallC where

import Data.Group
import Data.Matrix

-- Constructing small groups exercise 4
data G4 = E | A | B | C deriving (Eq,Show)

instance Semigroup G4 where
   E <> x = x
   x <> E = x
   A <> A = E  -- condition
   A <> B = C  
   A <> C = B   
   B <> A = C 
   B <> B = A  -- choice
   B <> C = E
   C <> A = B
   C <> B = E -- inverses
   C <> C = C


instance Monoid G4 where
   mempty = E

instance Group G4 where
   invert x = case x of
                   E -> E
                   A -> A
                   B -> C
                   C -> B

-- Note: SmallB.G4 and SmallC.G4 are different types and different groups
tableG4 :: [G4] -> Matrix G4
tableG4 els = fromList 4  4 [ x <> y | x <- els, y <- els]

{- *SmallC> tableG4 [E,A,B,C]
┌         ┐
│ E A B C │
│ A E C B │
│ B C A E │
│ C B E C │
└         ┘
-}

-- exercise 6
-- 4 elements one of which is the neutral one. Because not-equals,
-- which are each other's inverses, com in pairs, there can be at most
-- one such pair. Furthermore a <> b cannot be a or b (one would be the e)
-- so it must be the remaining c.
