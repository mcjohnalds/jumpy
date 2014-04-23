module Types where
import Data.Time
import System.Random
import Control.Lens
import Graphics.UI.SDL
import Graphics.UI.SDL.TTF
import Hitbox

data HAlign = HLeft | HCenter | HRight
data VAlign = VUp | VCenter | VDown

data Game = Game
  { _playerHitbox :: Hitbox
  , _playerXVel :: Double
  , _playerYVel :: Double
  , _playerJumpTime :: Maybe Double -- time a jump was initiated, is Nothing when not jumping
  , _playerAlive :: Bool
  , _playerDeathTime :: Double
  , _blocks :: [Hitbox]
  , _nextBlockGap :: Double
  , _score :: Integer
  , _shouldQuit :: Bool
  , _startUTCTime :: UTCTime
  , _time :: Double
  , _lastDrawTime :: Double
  , _font :: Font
  , _heldKeys :: [SDLKey]
  , _keyPresses :: [SDLKey]
  , _keyReleases :: [SDLKey]
  , _stdGen :: StdGen
  , _debug :: String -- always displayed when not null
  , _deltaTime :: Double
  , _atMenu :: Bool
  , _gameTime :: Double -- how long since the level was last started/restarted
  , _levelSpeed :: Double }

makeLenses ''Game