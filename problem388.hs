-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 388

allTriples :: [(Integer, Integer, Integer)]
allTriples = [(a, b, c) | a <- [1..], b <- [1..], c <- [1..]]

digitCount :: Integer -> Integer
digitCount n
  | abs n < 10 = 1
  | otherwise = 1 + digitCount (n `div` 10)

main = print (first * 10 ^ 9 + last)
  where first = n `div` 10 ^ k
        last = n `mod` 10 ^ 9
        k = digitCount n - 9
        n = fromIntegral (length (takeWhile (\(a, b, c) -> a <= max && b <= max && c <= max) 
                                            (takeWhile (\(a, b, c) -> gcd a (gcd b c) == 1) allTriples)))
        max = 10^10
