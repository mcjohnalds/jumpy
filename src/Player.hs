module Player where
import Graphics.UI.SDL
import Control.Monad.State.Lazy
import Control.Lens
import Data.Maybe
import Utils
import Types
import Hitbox

default (Double)

initialPlayerX = 100
initialPlayerY = 100
playerColor = 0x10F0F0
playerWidth = 32
playerHeight = 64
playerMoveVel = 600
playerJumpVel = -650
playerIdleVel = -initialLevelSpeed / 2 -- how much we move when not running left or right
maxPlayerJumpTime = 0.3 -- how long we can hold the jump key

resetPlayer :: State Game ()
resetPlayer = do
  playerHitbox .= Hitbox initialPlayerX initialPlayerY playerWidth playerHeight
  playerXVel .= 0
  playerYVel .= 0
  playerJumpTime .= Nothing
  playerAlive .= True

updatePlayer :: State Game ()
updatePlayer = do
  s <- get
  let p = s^.playerHitbox
      dt = s^.deltaTime
      hk = s^.heldKeys

  -- x movement
  when (SDLK_a `elem` hk) $ playerHitbox.hbX -= playerMoveVel * dt
  when (SDLK_d `elem` hk) $ playerHitbox.hbX += playerMoveVel * dt
  playerHitbox.hbX += playerIdleVel * dt
  playerHitbox.hbX %= clamp 0 (screenWidth - playerWidth)

  -- jumping & ground collisions
  let collidable = filter (\b -> horizCollision p b && bottomSide p < bottomSide b) $ s^.blocks -- potential things we might fall on
      highestSide = if null collidable then screenWidth * 2 else _hbY $ highest collidable
      jumpTime = (s^.time) - fromJust (s^.playerJumpTime)
      canFly = isJust (s^.playerJumpTime) && jumpTime <= maxPlayerJumpTime
      touchingGround = p^.hbY + playerHeight >= highestSide
  when touchingGround $ playerYVel .= 0
  when (SDLK_w `elem` s^.keyPresses && touchingGround) $ do
    playerJumpTime .= Just (s^.time)
    playerYVel .= playerJumpVel
  when (SDLK_w `elem` hk && canFly) $ playerYVel .= playerJumpVel
  when (SDLK_w `elem` s^.keyReleases) $ playerJumpTime .= Nothing

  playerYVel += gravityAccel * dt
  playerHitbox.hbY += s^.playerYVel * dt
  playerHitbox.hbY %= min (highestSide - playerHeight)

  when (s^.playerAlive && p^.hbY > screenHeight) killPlayer

killPlayer :: State Game ()
killPlayer = do
  s <- get
  playerAlive .= False
  playerDeathTime .= s^.time

drawPlayer :: Game -> IO ()
drawPlayer Game{_playerHitbox=Hitbox{..}, ..} = if _playerAlive
  then rect _hbX _hbY _hbWidth _hbHeight $ Pixel playerColor
  else text HCenter VCenter messageX messageY _font fontColor "YOU'RE DEAD :("