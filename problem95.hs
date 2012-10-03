properDivisors :: Integer -> [Integer]
properDivisors n = [d | d <- [1..n-1], n `mod` d == 0]

sumProperDivisorsSeq :: Integer -> [Integer]
sumProperDivisorsSeq n = n : sumProperDivisorsSeq (sum (properDivisors n))

amicableChain :: Integer -> [Integer]
amicableChain n = amicableChain' (sumProperDivisorsSeq n)
  where amicableChain' (x:xs) = x : takeWhile (\n -> (n /= x) && (n /= 0)) xs

main = print [take 10 (amicableChain n) | n <- [1..1000]]