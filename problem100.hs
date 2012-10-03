-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 100

import Data.Ratio

-- Calculate the probability of drawing k of the r items out of a total of n items without replacement
prob :: Integer -> Integer -> Integer -> Rational
prob 1 r n = r % n
prob k r n = r % n * (prob (k - 1) (r - 1) (n - 1))

main = do
  print (prob 2 15 21)
  print (prob 2 85 120)