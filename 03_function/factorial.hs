fact 0 = 1
fact n = n * fact(n-1)

pow :: Num a => a -> a
pow x = x * x 

prog = [1..10]

main = do
  print prog
  print(map pow prog)
  print(map fact prog)
  
