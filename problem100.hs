import Data.Ratio

-- Calculate the probability of drawing k of the r items out of a total of n items without replacement
prob :: Integer -> Integer -> Integer -> Rational
prob 1 r n = r % n
prob k r n = r % n * (prob (k - 1) (r - 1) (n - 1))

main = do
  print (prob 2 15 21)
  print (prob 2 85 120)