class Monad' m where
  (>>>=) :: m a -> (a -> m b) -> m b
  infixl 1 >>>=

  return' :: a -> m a

  (>>>) :: m a -> m b -> m b
  (>>>) m f = m >>>= \_ -> f
  infixl 1 >>>


data Maybe' a = Nothing' | Just' a deriving Show
instance Monad' Maybe' where 
  (>>>=) Nothing' _ = Nothing'
  (>>>=) (Just' x) f = f x
  return' a = Just' a
  (>>>) m f = m >>>= \_ -> f

divn :: (Integral a) => a -> a -> Maybe' a
divn n x 
  |x `mod` n == 0 = Just' ( x `div` n )
  |otherwise = Nothing'

div2 :: (Integral a) => a -> Maybe' a
div2 = divn 2 

div8 :: (Integral a) => a -> Maybe' a
div8 x = return' x >>>= div2 >>>= div2 >>>= div2 

main = do
  print $ div8 10
  print $ div8 96
  print $ div8 64
  
