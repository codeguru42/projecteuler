-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 1

sumN n = n * (n + 1) `div` 2
sumMultiples n m = m * sumN (n `div` m)

main = do {
    print (sumMultiples 999 3);
    print (sumMultiples 999 5);
    print (sumMultiples 999 15);
    print ((sumMultiples 999 3) + (sumMultiples 999 5) - (sumMultiples 999 15));
  }