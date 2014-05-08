-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 89

fromRomanNumerals :: String -> Int
fromRomanNumerals "" = 0
fromRomanNumerals ('I':'V':ns) =    4 + fromRomanNumerals ns
fromRomanNumerals ('I':'X':ns) =    9 + fromRomanNumerals ns
fromRomanNumerals ('V':ns)     =    5 + fromRomanNumerals ns
fromRomanNumerals ('I':ns)     =    1 + fromRomanNumerals ns
fromRomanNumerals ('X':'L':ns) =   40 + fromRomanNumerals ns
fromRomanNumerals ('X':'C':ns) =   90 + fromRomanNumerals ns
fromRomanNumerals ('X':ns)     =   10 + fromRomanNumerals ns
fromRomanNumerals ('L':ns)     =   50 + fromRomanNumerals ns
fromRomanNumerals ('C':'D':ns) =  400 + fromRomanNumerals ns
fromRomanNumerals ('C':'M':ns) =  400 + fromRomanNumerals ns
fromRomanNumerals ('C':ns)     =  100 + fromRomanNumerals ns
fromRomanNumerals ('D':ns)     =  500 + fromRomanNumerals ns
fromRomanNumerals ('M':ns)     = 1000 + fromRomanNumerals ns

toMinimalRomanNumeral :: Int -> String
toMinimalRomanNumeral = undefined

main = do
    content <- readFile "input/roman.txt"
    let ls = lines content
    let ns = map fromRomanNumerals ls
    mapM_ print $ ns
