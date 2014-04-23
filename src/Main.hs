import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State.Lazy
import System.Random
import Data.Time.Clock
import Data.List
import Utils
import Types
import Game

frameRate = 1/60

main = do
  -- window
  SDL.init [InitEverything]
  setVideoMode (floor screenWidth) (floor screenHeight) 32 [HWSurface, DoubleBuf]
  setCaption "Jumpy" []

  -- font
  TTF.init
  maybeFont <- tryOpenFont "DejaVuSans.ttf" 25
  when (isNothing maybeFont) $ error "couldn't load font"

  currentUTCTime <- getCurrentTime
  stdGen <- getStdGen

  loop Game
    { _shouldQuit = False
    , _startUTCTime = currentUTCTime
    , _time = 0
    , _lastDrawTime = 0
    , _font = fromJust maybeFont
    , _heldKeys = []
    , _stdGen = stdGen
    , _debug = ""
    , _atMenu = True }

loop :: Game -> IO Game
loop game@Game{..} = do
    currentUTCTime <- getCurrentTime
    events <- unfoldWhileM (/= NoEvent) pollEvent

    let time = currentUTCTime `diffTime` _startUTCTime
        shouldDraw = time - _lastDrawTime >= frameRate
        willQuit = Quit `elem` events || _shouldQuit
        releasedKeys = mapMaybe filterKeyUp events
        pressedKeys = mapMaybe filterKeyDown events
        heldKeys = (_heldKeys ++ pressedKeys) \\ releasedKeys
        game' = execState Game.update $ game
          { _time = time
          , _lastDrawTime = if' shouldDraw currentTime _lastDrawTime
          , _heldKeys = heldKeys
          , _keyPresses = pressedKeys
          , _keyReleases = releasedKeys
          , _deltaTime = time - _time }
    when shouldDraw $ drawEverything game'

    if willQuit then return game' else loop game'
 where
  filterKeyDown (KeyDown (Keysym k _ _)) = Just k
  filterKeyDown _ = Nothing

  filterKeyUp (KeyUp (Keysym k _ _)) = Just k
  filterKeyUp _ = Nothing