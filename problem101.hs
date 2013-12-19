-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 101

type Polynomial = [Integer]

eval :: Polynomial -> Integer -> Integer
eval p x = sum $ zipWith (*) p (map (x^) [0..])

fit :: Polynomial -> [Integer] -> Integer
fit poly sequence = fst . head . filter (\(x,y)->x/=y) $ zip sequence gen
    where gen = map (eval poly) [1..]

main = do
    print sequence
    print $ fit poly sequence
    where gen = [1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1]
          sequence = (take 11 (map (eval gen) [1..]))
          poly = [1]
