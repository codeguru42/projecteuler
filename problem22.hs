-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 22

import Data.Char
import System.IO
import Data.List

split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s
          
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar ch (c:cs)
    | c == ch   = removeChar ch cs
    | otherwise = c:(removeChar ch cs)

charScore :: Char -> Integer
charScore c = fromIntegral (ord c - ord 'A' + 1)

wordScore :: String -> Integer
wordScore s = sum (map charScore s)

main = do
    handle <- openFile "input/names.txt" ReadMode
    contents <- hGetContents handle
    let names = sort (map (removeChar '"') (split ',' contents))
    let nameScores = zipWith (*) [1..] (map wordScore names)
    print (sum nameScores)
    hClose handle