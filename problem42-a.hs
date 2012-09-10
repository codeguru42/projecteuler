-- Problem 42

import System.IO
import ProjectEuler

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangleNumber :: Integer -> Bool
isTriangleNumber k = (discriminant >= 0)
    && (isSquare discriminant)
    && round (-1 + (sqrt . fromIntegral $ discriminant)) `mod` 2 == 0
    where discriminant = 1 + 8 * k

isSquare :: Integer -> Bool
isSquare k = (round .  sqrt . fromInteger $ k) ^ 2 == k

isTriangleWord :: String -> Bool
isTriangleWord = isTriangleNumber . wordScore

main = do
    handle <- openFile "input/words.txt" ReadMode
    contents <- hGetContents handle
    print (length (filter isTriangleWord (map (removeChar '"') (split ',' contents))))
    hClose handle
