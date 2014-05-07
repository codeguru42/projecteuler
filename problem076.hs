-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 76

partition 0 = 1
partition n 
    | n < 0 = 0
    | otherwise = sum $ map (\k -> let n1 = n - k * (3*k - 1) `div` 2 
                                       n2 = n - k * (3*k + 1) `div` 2
                                   in (-1)^(k+1) * (partition n1 + partition n2)
                            ) [1..n]

main = print $ partition 5 - 1
