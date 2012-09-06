-- Problem 69

countIf :: (Integral b) => [a] -> (a -> Bool) -> b
countIf [] p = 0
countIf (x:xs) p
  | p x = 1 + countIf xs p
  | otherwise = countIf xs p

isRelativelyPrime :: (Integral a) => a -> a -> Bool
isRelativelyPrime m n = gcd m n == 1

phi :: (Integral a, Integral b) => a -> b
phi n = countIf [1..(n - 1)] (isRelativelyPrime n)

maxRatio :: (Int, Int, Double) -> (Int, Int, Double) -> (Int, Int, Double)
maxRatio t1@(_, _, x) t2@(_, _, y)
  | x > y = t1
  | otherwise = t2

main = print 
         (foldl
           maxRatio
           (0, 0, 0.0)
           [(n, phi n, ratio) | n <- [2..1000000], let ratio = fromIntegral n / (fromIntegral (phi n))])