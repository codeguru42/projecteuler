-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 59

import Data.Bits (xor)
import Data.Char (isSpace, ord, chr, toLower, isAlpha)
import Data.List (maximumBy)
import Data.Function (on)
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))
import ProjectEuler (split)

letterCount :: [Int] -> Int
letterCount = length . filter (isAlpha . chr)

encrypt :: [Int] -> [Int] -> [Int]
encrypt key = zipWith xor $ cycle key

decrypt :: [Int] -> [Int] -> [Int]
decrypt = encrypt

-- This works for the current use case but does not, in general, correctly
-- trim leading and trailing whitespace as intended
trim :: String -> String
trim s = takeWhile (not . isSpace) $ dropWhile isSpace s

makesMaximum :: (Ord b) => (a -> b) -> [a] -> a
makesMaximum f xs = fst $ maximumBy (compare `on` snd) $ zip xs $ map f xs

sumAscii :: String -> Integer
sumAscii s = sum $ map (fromIntegral . ord) s

main = do
  hCrypt <- openFile "input/cipher1.txt" ReadMode
  cryptContents <- hGetContents hCrypt
  let cryptAscii = map read $ split ',' $ trim cryptContents :: [Int]
  let keys = map (map ord) [[x, y, z] | x <- cs, y <- cs, z <- cs] 
        where cs = ['a'..'z']
  let plainAscii = map (flip encrypt cryptAscii) keys
  let theOne = makesMaximum letterCount plainAscii
  print $ map chr theOne
  print $ sum theOne
  hClose hCrypt
