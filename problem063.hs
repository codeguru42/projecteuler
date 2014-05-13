-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 63

digitCount :: Integer -> Integer
digitCount n = 1 + (floor $ logBase 10 n')
    where n' = fromIntegral n :: Double

nthPowers :: Integer -> [Integer]
nthPowers n = map (^n) [1..9]

nDigitNthPowers :: Integer -> [Integer]
nDigitNthPowers n = takeWhile (\k -> digitCount k == n) . dropWhile (\k -> digitCount k < n) $ nthPowers n

main = print . length . concat $ map nDigitNthPowers [1..n]
    where n = 300
