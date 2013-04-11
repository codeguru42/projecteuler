-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 59

import Data.Bits(xor)

encrypt :: [Integer] -> [Integer] -> [Integer]
encrypt key = zipWith xor $ cycle key

decrypt :: [Integer] -> [Integer] -> [Integer]
decrypt = encrypt

dist :: (Num a) => [a] -> [a] -> a
dist xs ys = sum $ map abs $ zipWith (-) xs ys
