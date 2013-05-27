-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 59

import Data.Bits(xor)
import Data.Char(isSpace, ord, chr)
import Data.List(maximumBy, sortBy, insert, nub)
import Data.Function(on)
import System.IO(openFile, hGetContents, hClose, IOMode(ReadMode))
import ProjectEuler(split, count)

encrypt :: [Int] -> [Int] -> [Int]
encrypt key = zipWith xor $ cycle key

decrypt :: [Int] -> [Int] -> [Int]
decrypt = encrypt

dist :: (Num a) => [a] -> [a] -> a
dist xs ys = sum $ map abs $ zipWith (-) xs ys

trim :: String -> String
trim s = takeWhile (not . isSpace) $ dropWhile isSpace s

makesMaximum :: (Ord b) => (a -> b) -> [a] -> a
makesMaximum f xs = fst $ maximumBy (compare `on` snd) $ zip xs $ map f xs

-- 'insertAll xs l' inserts each element 'x' of 'xs' into the sorted list 'l' 
-- immediately before the first element of 'l' which is greater than 'x' if 'x'
-- does not already appear in 'l'
insertAll :: (Ord a) => [a] -> [a] -> [a]
insertAll xs l = nub $ foldl (flip ($)) l (map insert xs)

sumAscii :: String -> Integer
sumAscii s = sum $ map (fromIntegral . ord) s

main = do
  hFreqs <- openFile "input/frequencies.txt" ReadMode
  freqContents <- hGetContents hFreqs
  let engFreqs = map (\(c:f:[]) -> (c !! 0, (read f) :: Double))
                 $ map (split '\t') (lines freqContents)
  print engFreqs
  hClose hFreqs
  hCrypt <- openFile "input/cipher1.txt" ReadMode
  cryptContents <- hGetContents hCrypt
  let cryptAscii = map read $ split ',' $ trim cryptContents :: [Int]
  let keys = map (map ord) [[x, y, z] | x <- cs, y <- cs, z <- cs] 
        where cs = ['a'..'z']
  let plainAscii = map (flip encrypt cryptAscii) keys
  let freqs = map count plainAscii
  print $ take 5 $ map (sortBy (\(x,_) (y,_) -> compare x y)) freqs
  hClose hCrypt
