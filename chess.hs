module Main where

import Control.Monad

solution :: [[Int]]
solution = foldM place [] [1..8]

place :: [Int] -> a -> [[Int]]
place current _ = do
  new <- filter (safe current) [1..8]
  return (new : current)

safe :: [Int] -> Int -> Bool
safe xs x =
  and
   [ all (/= x) xs
   , all (/= x) (zipWith (+) xs [1..])
   , all (/= x) (zipWith (-) xs [1..])
   ]

main :: IO ()
main = do
  print solution

