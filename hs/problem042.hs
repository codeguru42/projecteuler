-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 42

import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))
import Data.List.Split (splitOn)
import ProjectEuler (wordScore, removeChar)

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangleNumber :: Integer -> Bool
isTriangleNumber k = (discriminant >= 0)
   && (isSquare discriminant)
   && round (-1 + (sqrt . fromIntegral $ discriminant)) `mod` 2 == 0
  where discriminant = 1 + 8 * k

isSquare :: Integer -> Bool
isSquare k = (k >= 0) && (round .  sqrt . fromIntegral $ k) ^ 2 == k

isTriangleWord :: String -> Bool
isTriangleWord = isTriangleNumber . wordScore

main = do
    handle <- openFile "../input/words.txt" ReadMode
    contents <- hGetContents handle
    print (length (filter isTriangleWord (map (removeChar '"') (splitOn "," contents))))
    hClose handle
