import Control.Monad
import Control.Monad.Writer


type Bird = Integer
type Pole = (Bird,Bird)

allowBird = 3

addBird :: (Bird,Bird) -> Pole -> Maybe Pole
addBird (birdl,birdr) (left,right) =
  if abs (left' - right') <= allowBird then Just (left',right') else Nothing
    where left' = left + birdl
          right' = right + birdr 

addBirds :: [(Bird,Bird)] -> Pole -> Maybe Pole
addBirds b p = foldM (flip addBird) p b

addBirdLog :: (Bird,Bird) -> Maybe Pole -> Writer [String] (Maybe Pole)
addBirdLog b p = writer (p >>= (flip addBird b), [show p])

addBirdsLog :: [(Bird,Bird)] -> Pole -> Writer [String] (Maybe Pole)
addBirdsLog b p = foldM (flip addBirdLog) (Just p) b


printPole :: Maybe Pole -> IO ()
printPole p = putStrLn $ "Now Birds...\n  " ++ (show' p)
  where show' (Just p') = show p'
        show' Nothing = "Failed..."

printHistory :: [String] -> IO ()
printHistory s = do
  putStrLn "History..."
  putStr $ unlines $ map ("  "++) s

printPoleHistory :: Writer [String] (Maybe Pole) -> IO ()
printPoleHistory w = let (p, h) = runWriter w
                     in do
                       printHistory h
                       printPole p
                       putStrLn ""

promptLine :: String -> IO String
promptLine s = do
  putStr s
  getLine

inputInteger :: String -> IO Integer
inputInteger s = do
  numStr <- promptLine s
  case reads numStr of
    [(n,"")] -> return n
    _ -> inputInteger "Try again: "

inputBird :: IO (Bird,Bird)
inputBird = do
  birdl <- inputInteger "How many birds is comming to left side? : "
  birdr <- inputInteger "How many birds is comming to right side? : "
  return (birdl,birdr)
  
dialogueCycle :: Writer [String] (Maybe Pole) -> IO ()
dialogueCycle w = do
  printPoleHistory w
  b <- inputBird
  putStrLn ""
  dialogueCycle $ w >>= (addBirdLog b)

main = do 
  --printPoleHistory $ addBirdsLog [(1,3),(2,1),(1,2),(3,1)] (0,0)
  --printPoleHistory $ addBirdsLog [(1,3),(2,1),(-1,2),(3,1)] (0,0)
  dialogueCycle $ return $ Just (0,0)



