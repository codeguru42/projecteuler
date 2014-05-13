-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 65

continuedFraction :: Double -> [Int]
continuedFraction x = n : continuedFraction (1 / (x - fromIntegral n))
    where n = floor x

main = print . take 25 . continuedFraction $ sqrt 2 
