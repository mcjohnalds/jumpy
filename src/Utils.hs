module Utils where
import Graphics.UI.SDL
import Graphics.UI.SDL.TTF
import Control.Monad
import Control.Monad.State.Lazy
import Data.Time.Clock
import System.Random
import Control.Lens
import Types

default (Double)

screenWidth = 800
screenHeight = 500
gravityAccel = 3400
initialLevelSpeed = 450
messageX = screenWidth / 2
messageY = screenHeight / 2
fontColor = Color 214 91 159

-- clamp between a lower and upper bound
clamp :: Ord a => a -> a -> a -> a
clamp lower upper x = min upper $ max lower x

-- draw a rectangle
rect :: RealFrac a => a -> a -> a -> a -> Pixel -> IO ()
rect x y w h p = do
  screen <- getVideoSurface
  void $ fillRect screen (Just $ Rect (floor x) (floor y) (floor w) (floor h)) p

-- draw text
text :: RealFrac a => HAlign -> VAlign -> a -> a -> Font -> Color -> String -> IO ()
text ha va x y f c s = draw ha va x y =<< renderTextSolid f s c

-- if-then-else as a function
if' :: Bool -> a -> a -> a
if' a b c = if a then b else c

-- difference between two times (as a Double)
diffTime :: UTCTime -> UTCTime -> Double
diffTime a b = realToFrac $ a `diffUTCTime` b

-- draw a surface to the screen
draw :: RealFrac a => HAlign -> VAlign -> a -> a -> Surface -> IO ()
draw ha va x y s = do
  screen <- getVideoSurface
  let w = fromIntegral $ surfaceGetWidth s
      h = fromIntegral $ surfaceGetHeight s

      getX HLeft = x
      getX HCenter = x - w / 2
      getX HRight = x + w

      getY VUp = y
      getY VCenter = y - h / 2
      getY VDown = y + h

      x' = getX ha
      y' = getY va
  void $ blitSurface s Nothing screen $ Just $ Rect (round x') (round y') 0 0

-- uses the state's _stdGen as the RNG
randomS :: Random a => State Game a
randomS = do
  p <- get
  let (r, gen) = random $ p^.stdGen
  stdGen .= gen
  return r

-- uses the state's _stdGen as the RNG
randomRS :: Random a => a -> a -> State Game a
randomRS lower upper = do
  p <- get
  let (r, gen) = randomR (lower, upper) $ p^.stdGen
  stdGen .= gen
  return r