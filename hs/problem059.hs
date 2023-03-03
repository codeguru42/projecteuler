-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 59

import Data.Bits (xor)
import Data.Char (isSpace, ord, chr, isAlpha)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))

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
makesMaximum f = head . sortBy (\x y -> compare (f y) (f x))

sumAscii :: String -> Integer
sumAscii s = sum $ map (fromIntegral . ord) s

main = do
  hCrypt <- openFile "../input/cipher1.txt" ReadMode
  cryptContents <- hGetContents hCrypt
  let cryptAscii = map read . splitOn "," $ trim cryptContents
      keys = [map ord [x, y, z] | x <- cs, y <- cs, z <- cs]
        where cs = ['a'..'z']
      plainAscii = map (flip encrypt cryptAscii) keys
      theOne = makesMaximum letterCount plainAscii
  print $ map chr theOne
  print $ sum theOne
  hClose hCrypt
