module Elprop where

import Data.Matrix
import Oetwo  -- use :set -i../Ch03/  to allow GHCi to find the module
import Data.Group

-- B Rules of Algebra in Groups

{- *Elprop> tableG6
┌             ┐
│ I A B C D K │
│ A I C B K D │
│ B K D A I C │
│ C D K I A B │
│ D C I K B A │
│ K B A D C I │
└             ┘
-}

-- exercise 1
-- if x² = e then x = e
-- counterexample: A² = I but A /= i

-- exercise 2
-- if x² = a² then x = a

-- *Elprop> getRow 1 tableG6
-- [I,A,B,C,D,K]                   list (vector) of elements
-- *Elprop> getDiag tableG6
-- [I,I,D,I,B,I]                   list (vector) of squares
-- counterexample: A² = C² but A /= C

--exercise 3
-- (ab)² = a²b²
-- counterexample: (AB)² = C² = I 
--             but A² = I, B² = D and (A²B²) = D

-- exercise 4
-- if x² = x then x = e
-- correct: xxx-1 = xx-1
--          xe = e
--          x = e

-- exercise 5
-- every element has a square root
-- counterexample: A is not in the diagonal

--exercise 6
-- for every x and y there is a z such that y is xz
-- correct: x-1 exists, x-1y exists and y = xx-1y


