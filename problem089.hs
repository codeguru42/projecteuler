-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 89

fromRomanNumeral :: String -> Int
fromRomanNumeral "" = 0
fromRomanNumeral ('I':'V':ns) =    4 + fromRomanNumeral ns
fromRomanNumeral ('I':'X':ns) =    9 + fromRomanNumeral ns
fromRomanNumeral ('V':ns)     =    5 + fromRomanNumeral ns
fromRomanNumeral ('I':ns)     =    1 + fromRomanNumeral ns
fromRomanNumeral ('X':'L':ns) =   40 + fromRomanNumeral ns
fromRomanNumeral ('X':'C':ns) =   90 + fromRomanNumeral ns
fromRomanNumeral ('X':ns)     =   10 + fromRomanNumeral ns
fromRomanNumeral ('L':ns)     =   50 + fromRomanNumeral ns
fromRomanNumeral ('C':'D':ns) =  400 + fromRomanNumeral ns
fromRomanNumeral ('C':'M':ns) =  400 + fromRomanNumeral ns
fromRomanNumeral ('C':ns)     =  100 + fromRomanNumeral ns
fromRomanNumeral ('D':ns)     =  500 + fromRomanNumeral ns
fromRomanNumeral ('M':ns)     = 1000 + fromRomanNumeral ns

toMinimalRomanNumeral :: Int -> String
toMinimalRomanNumeral = undefined

main = do
    content <- readFile "input/roman.txt"
    let ls = lines content
    mapM_ print $ map fromRomanNumeral ls
