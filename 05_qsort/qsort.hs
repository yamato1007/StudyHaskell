qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
--  where smaller = [y | y <- xs , y <= x]
--        larger = [y | y <- xs , y > x]
  where smaller = filter (<= x) xs
        larger = filter (> x) xs

list = [4,2,6,2,7,4,2,87,3,2,5,0,4,6,21,2,2,5,3,5]

main = print $ qsort list

