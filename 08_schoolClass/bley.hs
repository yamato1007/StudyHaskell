bin = [1,0,1,1,1,0,1,0,0,1]

exor 0 0 = 0
exor 0 1 = 1
exor 1 0 = 1
exor 1 1 = 0

makeBray bin@(x:xs) = zipWith exor bin (0:bin)

makeBin bray = scanl1 exor bray

makeBin' bray@(x:xs) = x : repeat x xs
  where repeat _ [] = []
        repeat bin bray@(x':xs') = bin' : repeat bin' xs'
          where bin' = bin `exor` x'


main = do
  print bin
  print $ makeBray bin
  print $ makeBin $ makeBray bin
  print $ makeBin' $ makeBray bin
