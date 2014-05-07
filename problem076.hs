-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 76

partition n = p !! n
    where p = [p' n' | n' <- [0..n+1]]
          p' 0 = 1
          p' n = sum $ map (\k -> let n1 = n - k * (3*k - 1) `div` 2 
                                      n2 = n - k * (3*k + 1) `div` 2
                                      p1 = if n1 < 0 then 0 else p !! n1
                                      p2 = if n2 < 0 then 0 else p !! n2
                                  in (-1)^(k+1) * (p1 + p2)
                           ) [1..n]

main = print $ partition 100 - 1
