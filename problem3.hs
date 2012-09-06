-- Problem 3

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]

divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]

main = print (head (filter isPrime (filter ((==0) . (n `mod`)) [n-1,n-2..])))
  where n = 600851475143