-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 384

toBinaryString :: Integer -> String
toBinaryString 0 = "0"
toBinaryString 1 = "1"
toBinaryString n = toBinaryString (n `div` 2) ++ toBinaryString (n `mod` 2)

a :: Integer -> Integer
a = a' . toBinaryString
  where a' "" = 0
        a' ('1':'1':bits) = 1 + a' ('1':bits)
        a' (_:bits) = a' bits

main = print $ zip (map a ns) (map toBinaryString ns)
       where ns = [0..45]
