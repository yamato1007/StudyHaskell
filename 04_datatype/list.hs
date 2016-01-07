list = [0,1,2,3,4,5,6,7,8,9]
str = "Hello, World!"

--listからbで指定した値を添え字とする値のリストを取得する
getFromList :: [Int] -> [x] -> [x]
getFromList [] _ = []
getFromList _ [] = []
{-
getFromList b list
  |headb >= length list = error "リストより大きい値を示している"
  |otherwise = (list !! headb) : (getFromList tailb list)
    where headb = head b
          tailb = tail b
-}
getFromList (b:bs) list 
  |b >= length list = error "list B have list value what is bigger than list A size"
  |otherwise = (list !! b) : (getFromList bs list)

main = do
  print list 
  print(head list)
  print(tail list)
  print(list ++ [11,13,15])  
  print(-1 : list)
  print(list !! 0)
  print(length list)
  print(getFromList [1,5,3,2,5,5,3,7,3] list)
  print(getFromList [10,0,2,2,9,5,10,3,7,3] str)
