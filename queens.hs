-- A rudimentary solver for the N queens problem
-- (c) ane <ane@iki.fi> 2009
-- (Assignment for Functional Programming 1 at JYU)

module Main where

nqueens :: Int -> [[Int]]
nqueens 0 = [[]]
nqueens n = foldl (addqueen n) [[]] [1..n]

addqueen :: Int -> [[Int]] -> Int -> [[Int]]
addqueen width prev_solutions index =
  let newcolumn = [1 .. width] in
  (concatMap
      (\solution ->
        map (: solution)
          (filter (`validSet` solution) newcolumn)) prev_solutions)

validSet :: (Num a, Enum a) => a -> [a] -> Bool
validSet newcolumn solution = all (\(column, row) -> column /= newcolumn && column + row /= newcolumn && column - row /= newcolumn) (zip solution [1 ..])

visualize ::  (Num a, Enum a) => [a] -> a -> [String]
visualize sol width = map mapLine (mapSolution sol width)
  where
    mapLine = map (\a -> if a then 'Q' else '.')

mapSolution :: (Num a, Enum a) => [a] -> a -> [[Bool]]
mapSolution solution width = map columnVisual solution
  where
    columnVisual idx = [ x == idx | x <- [1 .. width]]

formatBoard :: [[Int]] -> Int -> [String]
formatBoard solutions width = map (unlines . (("  " ++ take width ['A' .. 'Z']) : )) (indexRow (map (`visualize` width) solutions))
  where
    indexRow = map (\sol -> map (\(idx, row) -> idx : "|" ++ row) (zip ['A' ..] sol))

main = putStrLn (unlines (formatBoard (nqueens 20) 20))
