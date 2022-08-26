module Twoels where

{- Let a and b be the two elements. Then an operation f has 4 arguments,
 - (a,a),(a,b),(b,a) and (b,b). There are also 4 possible values (including 
 - equals). So, there are 16 possible value lists. Any match with specific
 - arguments is arbitrary, as long as it is the same for each value list.
 -}
 
-- exercise 1, with 'a' and 'b' as the two elements
vals :: [String]
vals = [ [x,y,u,v] | x <- ls, y <- ls, u <- ls, v <- ls]
            where ls = "ab"
{-*Twoels> vals
 - ["aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb","baaa",
 - "baab","baba","babb","bbaa","bbab","bbba","bbbb"]
-}

-- arbitrarily name the operations (with numbers)
namedVals :: [(Int,String)] 
namedVals = zip [1..] vals
{- *Twoels> namedVals
 - [(1,"aaaa"),(2,"aaab"),(3,"aaba"),(4,"aabb"),(5,"abaa"),(6,"abab"),
 - (7,"abba"),(8,"abbb"),(9,"baaa"),(10,"baab"),(11,"baba"),(12,"babb"),
 - (13,"bbaa"),(14,"bbab"),(15,"bbba"),(16,"bbbb")]
 -}

-----------------------------------------------------
-- exercise 2 

{- f is commutative iff  f(a,b) == f(b,a).
 - To know which value positions match this condition,
 - define an operation that matches with a list of values
-}
getVal :: String -> (Char,Char) ->  Char
getVal [x,y,u,v] (i,j ) = 
    case (i,j) of
      ('a','a') -> x
      ('a','b') -> y
      ('b','a') -> u
      ('b','b') -> v
      _  -> error "character not 'a' or 'b'"

-- use the above definition of getVal 
isComm :: [(Int,String)]
isComm = filter checomm namedVals where
             checomm (_,[x,y,u,v]) = y == u -- this depends on getVal!!
{- *Twoels> isComm
 - [(1,"aaaa"),(2,"aaab"),(7,"abba"),(8,"abbb"),
 - (9,"baaa"),(10,"baab"),(15,"bbba"),(16,"bbbb")]
-}

-----------------------------------------------------
-- exercise 3

{- define an argument list, so that each position in a
 -  value list matches the right argument with getVal.
 -}
domain :: [(Char,Char)]
domain = [('a','a'),('a','b'),('b','a'),('b','b')]
dompair :: (String,String)
dompair = unzip domain

-- from a value list, which determines a specific operation,
-- and a domain, get the specific values for the arguments
funDom :: String -> [(Char,Char)] -> String
funDom fls dm =  map (getVal fls) dm

leftside,rightside :: String -> Char -> String
-- calculate (f (f (i,j), k) 
leftside fls z = funDom fls (zip fls (repeat z))
-- calculate f (i, f (j,k))
rightside fls z = funDom  fls (zip (fst dompair) res1) where
                    res1 = funDom fls (zip (snd dompair) (repeat z))

-- test if a function is associative
assoc :: String -> Bool
assoc fls  = leftside fls 'a' == rightside fls 'a' &&
             leftside fls 'b' == rightside fls 'b' 
 
-- get the associative functions
isAssoc :: [(Int,String)] 
isAssoc = filter (assoc . snd)  namedVals 
{- *Twoels> isAssoc
 - [(1,"aaaa"),(2,"aaab"),(4,"aabb"),(6,"abab"),
 - (7,"abba"),(8,"abbb"),(10,"baab"),(16,"bbbb")]
-}
 
-----------------------------------------------------
-- exercise 4

{- if an operation has a neutral element, it is either 'a' or 'b'.
 - If 'a': f('a','a') is 'a',
 -         f ('a','b') is 'b',
 -         f('b','a') is 'b',
 -         f ('b',b') can be anything
 -
 -         Analogous for 'b'
 -}
neutral :: String -> Bool
neutral fls = init fls == "abb" || tail fls == "aab"
-- get the functions with neutral elements
hasNeutral :: [(Int,String)]
hasNeutral =  filter (neutral . snd) namedVals                
{- *Twoels> hasNeutral
 - [(2,"aaab"),(7,"abba"),(8,"abbb"),(10,"baab")]
-}

-----------------------------------------------------
-- exercise 5

{- if 'a' is the neutral element,
 - then if 'b' has an inverse
 - f('b','b') must be 'a'
-}
hasInverse = filter (inverse . snd) hasNeutral where
               inverse fls = fls == "abba" || fls == "baab"
{- *Twoels> hasInverse
 - [(7,"abba"),(10,"baab")]
-}
-----------------------------------------------------
