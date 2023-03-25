module Utils where

indexOf :: Eq a => [a] -> a -> Int
indexOf arr x = indexOf' arr x 0
  where
    indexOf' (a : as) x ind
      | a == x = ind
      | otherwise = indexOf' as x (ind + 1)

pseudoRand :: Float -> Float
pseudoRand x = abs $ sin (1337_6.9_420 * x)