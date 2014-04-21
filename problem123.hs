-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 123

import ProjectEuler

main = do
    let mods = map (\(n, p) -> ((p - 1) ^ n + (p + 1) ^ n) `mod` (p*p)) $ zip [1..] primes
    print . head . dropWhile (\x -> snd x <10^10) $ zip [1..] mods
