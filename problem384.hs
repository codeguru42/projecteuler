-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 384

import Data.List (elemIndex)

toBinaryString :: Integer -> String
toBinaryString 0 = "0"
toBinaryString 1 = "1"
toBinaryString n = toBinaryString (n `div` 2) ++ toBinaryString (n `mod` 2)

a :: Integer -> Integer
a = a' . toBinaryString
  where a' "" = 0
        a' ('1':'1':bits) = 1 + a' ('1':bits)
        a' (_:bits) = a' bits

b :: Integer -> Integer
b n = (-1) ^ (a n)

s :: Integer -> Integer
s n = sum [b i | i <- [0..n]]

ss :: [Integer]
ss = map s [1..]

g :: Integer -> Integer -> Integer
g t c = fromIntegral $ g' ss t c
  where g' ss t 1 = let Just k = elemIndex t ss in k + 1
        g' ss t c = k + 1 + g' (drop (k + 1) ss) t (c - 1)
                    where Just k = elemIndex t ss

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = do
  print ns
  print $ map toBinaryString ns
  print $ map a ns
  print $ map b ns
  print $ take 50 ss
  print $ g 3 3
  print $ g 4 2
  print $ g 54321 12345
       where ns = [0..45]
