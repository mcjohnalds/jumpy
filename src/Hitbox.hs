module Hitbox where
import Control.Lens

data Hitbox = Hitbox
  { _hbX :: Double
  , _hbY :: Double
  , _hbWidth :: Double
  , _hbHeight :: Double }

isLeftOf :: Hitbox -> Hitbox -> Bool
isLeftOf a b = rightSide a < _hbX b

isRightOf :: Hitbox -> Hitbox -> Bool
isRightOf a b = _hbX a > rightSide b

isAbove :: Hitbox -> Hitbox -> Bool
isAbove a b = bottomSide a < _hbY b

isBelow :: Hitbox -> Hitbox -> Bool
isBelow a b = _hbY a > bottomSide b

leftmost :: [Hitbox] -> Hitbox
leftmost = foldl1 (\a b -> if isLeftOf a b then a else b)

rightmost :: [Hitbox] -> Hitbox
rightmost = foldl1 (\a b -> if isRightOf a b then a else b)

highest :: [Hitbox] -> Hitbox
highest = foldl1 (\a b -> if isAbove a b then a else b)

lowest :: [Hitbox] -> Hitbox
lowest = foldl1 (\a b -> if isBelow a b then a else b)

rightSide :: Hitbox -> Double
rightSide (Hitbox x _ w _) = x + w

bottomSide :: Hitbox -> Double
bottomSide (Hitbox _ y _ h) = y + h

-- detect collisions between two hitboxes on the x axis
horizCollision :: Hitbox -> Hitbox -> Bool
horizCollision a b = not $ isLeftOf a b || isRightOf a b

vertCollision ::  Hitbox -> Hitbox -> Bool
vertCollision a b = not $ isAbove a b || isBelow a b

makeLenses ''Hitbox