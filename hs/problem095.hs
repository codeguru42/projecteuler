-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

properDivisors :: Integer -> [Integer]
properDivisors n = [d | d <- [1..n-1], n `mod` d == 0]

sumProperDivisorsSeq :: Integer -> [Integer]
sumProperDivisorsSeq n = n : sumProperDivisorsSeq (sum (properDivisors n))

amicableChain :: Integer -> [Integer]
amicableChain n = amicableChain' (sumProperDivisorsSeq n)
  where amicableChain' (x:xs) = x : takeWhile (\n -> (n /= x) && (n /= 0)) xs

main = print [take 10 (amicableChain n) | n <- [1..1000]]