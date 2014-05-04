-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 81

import Control.Applicative ((<$>))
import Data.List.Split (splitOn)

main = do
    contents <- readFile "input/matrix.txt"
    let xs = map (read <$>) . map (splitOn ",") $ lines contents :: [[Int]]
    mapM_ print xs
