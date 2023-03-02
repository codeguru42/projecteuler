-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 101

type Polynomial = [Integer]

eval :: Polynomial -> Integer -> Integer
eval p x = sum $ zipWith (*) p (map (x^) [0..])

main = print (take 10 (map (eval gen) [1..]))
    where gen = [1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1]
