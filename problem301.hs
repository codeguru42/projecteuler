-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 301

import Data.Bits (xor)

type NimHeaps = (Int, Int, Int)

x :: NimHeaps -> Bool
x (a, b, c) = a `xor` b `xor` c /= 0

main = do
    print . length $ filter x [(i, 2*i, 3*i) | i <- [1..n]]
    where n = 2 ^ 30
