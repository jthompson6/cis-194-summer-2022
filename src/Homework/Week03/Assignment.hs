module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where


skips :: [a] -> [[a]]
-- my version
--skips xs = (map (\n -> (map (\(i, x) -> x) (filter (\(i, x) -> (mod i n == 0)) (zip [1 .. length xs] xs)))) [1 .. length xs])
-- with where
skips xs = (map (\n -> (map (\(i, x) -> x) (filter (\(i, x) -> (mod i n == 0)) (zip indices xs)))) indices) where indices = [1 .. length xs] 
-- ugh
-- skips xs = (map (\n -> (map (\(i, x) -> x) (filter (\(i, x) -> (mod i n == 0)) (zip s xs)))) s) where s = [1 .. length xs] 

-- #2
-- zip the list with itself offset by 1 and 2 spaces (accomplished by dropping elements). 
-- zip will automatically trailing unmatched values.
-- this is fine, as the first and last values can't be local maxima.
-- then filter for tuples where the middle value is bigger than the other two.
-- then map the list of tuples to get the middle value of each one.
localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : cs) = map (\((x, y), z) -> y) (filter (\((x, y), z) -> (y > x) && (y > z))  (zip (zip (a : b : cs) (b : cs)) cs))
localMaxima _ = []
-- originally tried this, with padding on either side for offsets. 
-- only works for ints, not integers, as we needs an upper bound
-- localMaxima l = map (\((x, y), z) -> y) (filter (\((x, y), z) -> (y > x) && (y > z))  (zip (zip (mx: mx : l) (mx: l)) l)) where mx = maxBound :: Int

-- #3
histogram :: [Integer] -> String
histogram = undefined
