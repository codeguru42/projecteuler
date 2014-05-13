-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 65

import Test.HUnit.Base (Test(..), (~:), (~=?))
import Test.HUnit.Text (runTestTT)

continuedFraction :: Double -> [Int]
continuedFraction x = n : continuedFraction (1 / (x - fromIntegral n))
    where n = floor x

eContinuedFraction :: [Int]
eContinuedFraction = 2 : 1 : 2 : [e' n | n <- [3..]]
    where e' n | n `mod` 3 == 2 = (eContinuedFraction !! (n - 3)) + 2
               | otherwise = 1

testEContinuedFraction :: Test
testEContinuedFraction = "Test eContinuedFraction" ~: expected ~=? actual
    where expected = [2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, 1, 1, 12, 1, 1, 14, 1, 1, 16, 1]
          actual   = take 25 eContinuedFraction

main = print . take 25 $ eContinuedFraction
