module Game where
import Graphics.UI.SDL as SDL
import Control.Monad.State.Lazy
import Control.Lens

import Utils
import Types
import Player
import Blocks

startMessage = "W,A,S,D TO MOVE. PRESS ANY KEY TO START"
backgroundColor = 0x000000
levelSpeedSlowMult = 1.5 -- gradually slow levelSpeed down this much on death
deathPauseTime = 4.5 -- time to wait after death before restarting the level

update :: State Game ()
update = do
  s <- get
  gameTime += s^.deltaTime
  if s^.atMenu then updateMenu else updateGame
  when (SDLK_ESCAPE `elem` (s^.heldKeys)) $ shouldQuit .= True

updateGame :: State Game ()
updateGame = do
  s <- get
  if s^.playerAlive
    then do
      updatePlayer
      levelSpeed .= initialLevelSpeed + 20 * (s^.gameTime) ** 0.6
      score .= floor (5 * (s^.gameTime) ** 1.3)
    else do
      let deathTime = (s^.time) `diffTime` (s^.playerDeathTime)
      when (deathTime > deathPauseTime) resetLevel
      levelSpeed -= s^.levelSpeed * deathTime * levelSpeedSlowMult * s^.deltaTime -- gradually slow down
  updateBlocks

updateMenu :: State Game ()
updateMenu = do
  s <- get
  when (lengthOf each (s^.heldKeys) > 0) $ do
    atMenu .= False
    resetLevel

resetLevel :: State Game ()
resetLevel = do
  resetPlayer
  resetBlocks
  gameTime .= 0
  levelSpeed .= initialLevelSpeed
  score .= 0

drawEverything :: Game -> IO ()
drawEverything g@Game{..} = do
  rect 0 0 screenWidth screenHeight $ Pixel backgroundColor
  if _atMenu then drawMenu g else drawGame g
  unless (null _debug) $ text HLeft VUp 10 10 _font fontColor _debug
  SDL.flip =<< getVideoSurface

drawGame :: Game -> IO ()
drawGame g@Game{..} = do
  mapM_ drawBlock _blocks
  drawPlayer g
  text HCenter VUp (screenWidth/2) 10 _font fontColor $ show _score

drawMenu :: Game -> IO ()
drawMenu g@Game{..} = text HCenter VCenter messageX messageY _font fontColor startMessage
