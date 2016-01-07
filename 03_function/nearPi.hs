nearPi :: Integer -> Double
nearPi maxtimes = 4 / (1 + recursion 0 maxtimes)
  where recursion :: Integer -> Integer -> Double
        recursion n times 
          |n == times = 0
          |otherwise = pow(getOddNum n) / (2 + recursion (n+1) times)



getOddNum n
  |n < 0 = error "引数が負"
  |n == 0 = 1
  |n > 0 = 2 + getOddNum(n-1)

pow x = x * x

main = do
  print (nearPi 1000)
