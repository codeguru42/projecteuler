module ProjectEuler

-- All divisors of a number 
divisors :: Integer -> [Integer] 
divisors 1 = [1] 
divisors n = firstHalf ++ secondHalf 
    where firstHalf = filter (divides n) (candidates n) 
          secondHalf = filter (\d -> n `div` d /= d) (map (n `div`) (reverse firstHalf)) 
          candidates n = takeWhile (\d -> d * d <= n) [1..n]  

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]

primeDivisors :: Integer -> [Integer]
primeDivisors n = [d | d <- divisors n, isPrime d]
