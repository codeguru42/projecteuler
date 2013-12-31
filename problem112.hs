-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 112

import ProjectEuler

isIncreasingNumber :: Integer -> Bool
isIncreasingNumber n = isIncreasingNumber' $ digits n
    where isIncreasingNumber' [x] = True
          isIncreasingNumber' (x:y:xs) = x >= y && isIncreasingNumber' (y:xs)

isDecreasingNumber :: Integer -> Bool
isDecreasingNumber n = isDecreasingNumber' $ digits n
    where isDecreasingNumber' [x] = True
          isDecreasingNumber' (x:y:xs) = x <= y && isDecreasingNumber' (y:xs)

isBouncyNumber :: Integer -> Bool
isBouncyNumber n = not (isIncreasingNumber n || isDecreasingNumber n)

bouncyNumbers :: Integer -> [Integer]
bouncyNumbers n = filter isBouncyNumber [1..n]

main = do
    print . head $ [n | n <- [1..], fromIntegral (length (bouncyNumbers n)) >= 0.5 * fromIntegral n]
