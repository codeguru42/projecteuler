-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 104

import ProjectEuler
import Data.List (sort)

fib :: [Integer]
fib = 0 : 1 : fib' fib
    where fib' (x:y:xs) = (x + y) : fib' (y:xs)

isPandigital :: Integer -> Bool
isPandigital n = sort (digits n) == [1..9]

isTrailingPandigital :: Integer -> Bool
isTrailingPandigital n = isPandigital $ n `mod` 10 ^ 9

isLeadingPandigital :: Integer -> Bool
isLeadingPandigital n = sort (take 9 (reverse (digits n))) == [1..9]

main = do
    print . head
          . map fst
          . filter (isLeadingPandigital . snd)
          $ zip [0..] fib
