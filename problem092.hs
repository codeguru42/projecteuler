-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 92

import ProjectEuler

squareDigits :: Int -> Int
squareDigits n = sum . map (\x -> x * x) $ digits n

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x
                   : if not $ p x
                     then takeUntil p xs
                     else []

main = print . sum $ map ((\x -> if x == 89 then 1 else 0) . last) chains
    where chain = takeUntil (\x -> x == 1 || x == 89)
                        . iterate squareDigits
          chains = map chain [2..n]
          n = 100000
