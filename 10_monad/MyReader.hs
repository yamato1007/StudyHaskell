module MyReader where

newtype MyReader a b = MyReader {runMyReader :: a -> b}

instance Functor (MyReader a) where
  fmap f (MyReader a) = MyReader $ \r -> f $ a r 

instance Applicative (MyReader a) where
  pure x = MyReader (\_ -> x)
  (MyReader f) <*> (MyReader a) = MyReader $ \r -> let ans = a r
                                                       calc = f r 
                                                   in calc ans 

instance Monad (MyReader a) where
  return x = MyReader (\_ -> x)
  (MyReader m) >>= f = MyReader $ \r -> let a = m r
                                            (MyReader nm) = f a
                                        in nm r
                                        
checkPrime :: Integer -> Bool
checkPrime x
  |x <= 1 = False
  |x == 2 = True
  |otherwise = all ((/=0).(x `mod`)) odd
    where odd = 2:[3,5..(truncate $ sqrt $ fromInteger x)]

rCalc :: MyReader Integer (Integer,Integer,Integer,Integer,Bool)
rCalc = do
  num <- MyReader id
  double <- MyReader (*2)
  square <- MyReader (^2)
  power2 <- MyReader (2^)
  prime <- MyReader checkPrime
  return (num,double,square,power2,prime)

main = do 
  print $ runMyReader (fmap (+1) (MyReader (*2))) 2
  print $ runMyReader ((+) <$> (MyReader (*2)) <*> (MyReader (+10))) 1 
  mapM_ print $ map (runMyReader rCalc) [1..10]
