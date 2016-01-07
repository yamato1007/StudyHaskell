module Main where

class Ord a => Vector a where
  size :: a -> Double

data Point = Point {x_axis :: Double, y_axis :: Double} deriving Show

instance Eq Point where
  (==) (Point x y) (Point x' y') = x == x' && y == y'

instance Ord Point where
  compare p1 p2
    |s1 == s2 = EQ
    |s1 < s2 = LT
    |otherwise = GT
      where s1 = size p1
            s2 = size p2

instance Vector Point where
  size (Point x y) = sqrt $ (square x) + (square y)


pow :: (Num x , Integral n) => n -> x -> x
pow 0 _ = 1
pow n x = x * (pow (n-1) x)

square :: Num x => x -> x
square = pow 2


main = do
  let
    point1 = Point 10 10
    point2 = Point 14 64
  print $ point1
  print $ point2
  print $ point1 == point2
  print $ point1 /= point2
  print $ size point1
  print $ size point2
  print $ point1 < point2
