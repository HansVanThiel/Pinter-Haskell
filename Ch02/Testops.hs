module Testops where

import Test.QuickCheck -- (if needed) :set -package QuickCheck in ghci 
import Test.Invariant (commutative, associative)

-- avoid incorrect results due to rounding errors
isEq :: Double -> Double -> Bool
isEq x y = abs( x-y ) <= 1.0e-5

-- restate equality
newtype Test = T Double
        deriving (Show )
instance Eq Test where
   (T x) == (T y ) =  x `isEq` y

-- restate operations for (T Double)
instance Num Test where
   (T x) + (T y) = T (x + y)
   (T x) - (T y) = T (x - y)
   (T x) * (T y) = T (x * y)
   abs (T x) = T (abs x)
   signum (T x) = T (signum x)
   fromInteger n = T ((fromInteger n)::Double)
instance Fractional Test where
   fromRational q = T ((fromRational q)::Double)
   (T x) / (T y) = T (x / y)

-- use the test values from Double  
instance Arbitrary Test where
      arbitrary  =  arbitrary >>= (\x -> return (T x))

-- use commutative and associative from Test.Invariant
-- DIY test for unit (identity)
prop_unit :: (Test -> Test -> Test) ->
             Test -> Test ->
             Bool
prop_unit f e x = e `f` x == x && x `f` e == x 

-- DIY test for inverses
prop_inverse :: (Test -> Test -> Test) ->
                (Test -> Test) ->
                Test -> Test ->
                Bool
prop_inverse f inv e x = x `f` (inv x) == e && (inv x) `f` x == e

-- rewritten example from exercises 
ex:: Test -> Test -> Test
ex x y = x + y + (T 1)
-- helper functions for testing example
ex_e :: Test
ex_e = T (negate 1)
ex_inv :: Test -> Test
ex_inv x = negate x - (T 2)
{- note: use verboseCheck to see the test values
*Testops> quickCheck (commutative ex)
+++ OK, passed 100 tests.
*Testops> quickCheck (associative ex)
+++ OK, passed 100 tests.
*Testops> quickCheck (prop_unit ex ex_e)
+++ OK, passed 100 tests.
*Testops> quickCheck (prop_inverse ex ex_inv ex_e)
+++ OK, passed 100 tests.
-}

-- operations in section B
b1,b2,b3,b4,b5,b7 :: Test -> Test -> Test
b1 x y = x + 2 * y + (T 4)
b2 x y = x + 2 * y - x * y
b3 x y = abs(x + y)
b4 x y = abs(x - y)
b5 x y = x * y + (T 1)
b7 x y = (x * y) / (x + y + (T 1))

-- b7 in original form
dob7 :: Double -> Double -> Double
dob7 x y = (x * y) / (x + y +  1)
-- with and without restating equality in Test type
{-
*Testops> quickCheck (associative b7)
+++ OK, passed 100 tests.
*Testops> quickCheck (associative dob7)
*** Failed! Falsified (after 2 tests and 11 shrinks):    
-0.1
-0.1
0.1
-}

instance Ord Test where -- equality Test or smaller Double
   (T x) <= (T y) = (T x) == (T y) || x < y

-- max already defined because of Ord definition
b6 :: Test -> Test -> Test
b6 x y = max x y 
{- *Testops> quickCheck (commutative b6)
+++ OK, passed 100 tests.
*Testops> quickCheck (associative b6)
+++ OK, passed 100 tests.
-}

