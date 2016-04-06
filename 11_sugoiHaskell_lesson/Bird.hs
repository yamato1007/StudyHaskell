module Bird where

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

main = do 
  print $ runWriter $ addBirdsLog [(1,3),(2,1),(1,2),(3,1)] (0,0)
  print $ runWriter $ addBirdsLog [(1,3),(2,1),(-1,2),(3,1)] (0,0)
