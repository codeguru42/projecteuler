-- Problem 57

rootTwo :: (Integral a, Integral b) => a -> (b, b)
rootTwo 1 = (3, 2)
rootTwo n = (num + 2 * denom, num + denom)
  where (num, denom) = rootTwo (n - 1)

digitCount :: (Integral a, Integral b) => a -> b
digitCount n 
  | n >= 0 && n < 10 = 1
  | otherwise = 1 + digitCount (n `div` 10)

compareDigits m n = (digitCount m) > (digitCount n)

digitExceedCount :: (Integral a, Integral b) => a -> b
digitExceedCount 0 = 0
digitExceedCount i 
  | compareDigits num denom = 1 + digitExceedCount (i - 1)
  | otherwise = digitExceedCount (i - 1)
  where (num, denom) = rootTwo i
  
main = print (digitExceedCount 1000)