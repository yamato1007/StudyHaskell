myiterate :: (a -> a) -> a -> [a]
myiterate f a = a : iterate f (f a)

union :: Integer -> (a -> a) -> (a -> a)
union 0 _ = \x -> x
union n f = f.(union (n - 1) f )

twice :: Num a => a -> a
twice = (*2)

main = do
  print $ union 0 twice $ 2
  --print $ iteration twice 2
