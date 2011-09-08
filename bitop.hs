-- Decomposes a string into its bit representation
-- (c) ane <ane@iki.fi> 2009

import System
import Data.Bits
import Data.Char

getNthBit :: Int -> Int -> Int
getNthBit buf n
    | buf .&. (buf `shiftL` n) > 0 = 1
    | otherwise                  = 0

getBits :: Int -> Int -> [Int]
getBits byte count = map (getNthBit byte) [0 .. count]

bits :: String -> [[Int]]
bits = map (\x -> getBits (ord x) 7)

main = do args <- getArgs
          print (bits . concat $ args)
