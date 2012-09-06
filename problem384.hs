-- Problem 384

a :: Int -> Int
a n = a' (n / 2) (n `mod` 2)
  where a' n b
          | n == 0 = 0
          | n == 1 = 0
          | b == 1 && b' == 1 = 1 + a' n' b'
          | otherwise a' n' b'
            where b' = n `mod` 2
                  n' = n / 2
    
b :: Int -> Int
b n = (-1) ^ (a n)

s :: Int -> Int
s n = sum [b i | i <- [0..n]]

main = print [(n, a n, b n, s n) | n -> [0..7]]