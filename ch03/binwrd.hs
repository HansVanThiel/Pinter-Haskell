{-# LANGUAGE BinaryLiterals #-}
module Binwrd where

import Data.Word (Word8)
import Data.Bits
import Data.List (minimum)
import Numeric (showIntAtBase) -- for showing bitsequence
import Data.Char (intToDigit)

s x = showIntAtBase 2 intToDigit x ""

-- codewords C1
w0,w1,w2,w3,w4,w5,w6,w7 :: Word8
w0 = 0B00000
w1 = 0B00111
w2 = 0B01001
w3 = 0B01110
w4 = 0B10011
w5 = 0B10100
w6 = 0B11010
w7 = 0B11101

wrdls :: [Word8]
wrdls = [w0,w1,w2,w3,w4,w5,w6,w7]

-- check the  minimum distance (number of set bits) between words in a list
distCheck :: [Word8] -> Int
distCheck ls = minimum (map popCount difls) where
         difls = [x `xor` y | x <- ls, y <- ls, x /= y ]
{- *Binwrd> distCheck wrdls
 - 2
-}

-- exercise 1 
-----------------------------------------------------------
-- bits 7,6 and 5 are padding in Word8
-- bits 4,3 and 2 are information
-- bits 1 and 0 are parity bits
--  7 | 6 | 5 | 4 | 3 | 2 | 1 | 0
--  _ | _ | _ | 1 | 2 | 3 | 4 | 5
-----------------------------------------------------------

-- check a4 = a1 + a3 becomes: bit 1 = bit 4 xor bit 2
getBit :: Word8 -> Int -> Word8
getBit wrd n | testBit wrd n = 0B1
             | otherwise = 0B0

parity1,parity2 :: Word8 -> Bool
parity1 wrd = getBit wrd 1 == (getBit wrd 4) `xor` (getBit wrd 2)

parity2 wrd = getBit wrd 0 == a1 `xor` a2 `xor` a3 where
       a1 = getBit wrd 4
       a2 = getBit wrd 3
       a3 = getBit wrd 2
{- *Binwrd> all parity1 wrdls
 - True
 - *Binwrd> all parity2 wrdls
 - True
 -}
-----------------------------------------------------------
-- exercise 2

--  7 | 6 | 5 | 4 | 3 | 2 | 1 | 0    Word8
--  _ | _ | 1 | 2 | 3 | 4 | 5 | 6    B6 code

rawdat :: [Word8]
rawdat = [0..7]

-- data with 3 zero's to the right
shd :: [Word8]
shd = map ((flip shift) 3) rawdat

-- a4 = a2
addp1, addp2, addp3 :: Word8 -> Word8
addp1 w | testBit w 4 = setBit w 2
        | otherwise = w -- position 2 already is 0

-- a5 = a1 + a2
addp2 w | t == 0B1 = setBit w 1
        | otherwise = w
        where t = (getBit w 5) `xor` (getBit w 4)

-- a6 = a1 + a2 + a3
addp3 w | t == 0B1 = setBit w 0
        | otherwise = w
        where t = (getBit w 5) `xor` (getBit w 4) `xor` (getBit w 3)

addParity :: Word8 -> Word8
addParity = addp3 . addp2 . addp1

codeC2 :: [Word8]
codeC2 = map addParity shd

{- *Binwrd> map s codeC2
 - ["0","1001","10111","11110","100011","101010","110100","111101"]
 -
 - *Binwrd> distCheck codeC2
 - 2
 -}

-----------------------------------------------------------
-- exercise 3

shB4 :: [Word8]
shB4 = map ((flip shift) 2) [0..3]

{- *Binwrd> map s shB4
["0","100","1000","1100"]
-}

codeB4 :: [Word8]
codeB4 = [0B0,0B101,0B1010,0B1111]

{- *Binwrd> distCheck codeB4
 - 2
 -}
-----------------------------------------------------------
-- exercise 4

-- Note: no read or show for binary literals in Data.Bits
ew1,ew2,ew3,ew4,ew5,ew6 :: Word8
ew1 = 0B11111
ew2 = 0B00101
ew3 = 0B11000
ew4 = 0B10011
ew5 = 0B10001
ew6 = 0B10111

-- find correct words with 0 or 1 distance from the observed word
possWords :: Word8 -> [Word8] -> [Word8]
possWords ew  = filter (testWord ew) where 
    testWord :: Word8 -> Word8 -> Bool
    testWord ew w = popCount (ew `xor` w) <= 1

-- display them in binary format with s
showPossibles ew = map s (possWords ew wrdls)
{- *Binwrd> showPossibles ew1            
["11101"]                            w7     
*Binwrd> showPossibles ew2
["111"]                              w1  
*Binwrd> showPossibles ew3
["11010"]                            w6
*Binwrd> showPossibles ew4
["10011"]                            w4   !! no error
*Binwrd> showPossibles ew5
["10011"]                            w4   !! one error
*Binwrd> showPossibles ew6
["111","10011"]                      w1, w4
-}
-----------------------------------------------------------
-- no Haskell implementations for the remaining exercises
