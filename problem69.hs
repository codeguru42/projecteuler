-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 69

import ProjectEuler (phi)

maxRatio :: (Integer, Integer, Double) -> (Integer, Integer, Double) -> (Integer, Integer, Double)
maxRatio t1@(_, _, x) t2@(_, _, y)
  | x > y = t1
  | otherwise = t2

main = print (foldl
                maxRatio
                (0, 0, 0.0)
                [(n, phi n, ratio) | n <- [2..max], let ratio = fromIntegral n / (fromIntegral (phi n))]
              )
    where max = 1000000
