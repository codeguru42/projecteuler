-- Problem 61

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangle :: Integer -> Bool
isTriangle k = (discriminant >= 0)
   && (isSquare discriminant)
   && round (-1 + sqrt (fromIntegral discriminant)) `mod` 2 == 0
  where discriminant = 1 + 8 * k

square :: Integer -> Integer
square n = n ^ 2

isSquare :: Integer -> Bool
isSquare k = (round .  sqrt . fromInteger $ k) ^ 2 == k

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

heptagonal :: Integer -> Integer
heptagonal n = n * (5 * n - 3) `div` 2

octagonal :: Integer -> Integer
octagonal n = n * (3 * n - 2)

main = print (map isTriangle [1..25])