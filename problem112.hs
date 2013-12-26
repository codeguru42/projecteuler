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

main = do
    putStrLn "Increasing..."
    print $ filter isIncreasingNumber ns
    putStrLn "Decreasing..."
    print $ filter isDecreasingNumber ns
    where ns = [100..500]
