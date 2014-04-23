{-# LANGUAGE RecordWildCards #-}
module Blocks where
import Graphics.UI.SDL
import Control.Monad.State.Lazy
import Control.Lens
import Utils
import Types
import Hitbox as HB

default (Double, Int, Integer)

firstBlock = Hitbox 0 (screenHeight - 20) screenWidth 15
groundColor = 0x505050
minBlockGap = 100
maxBlockGap = 270
minBlockWidth = 100
maxBlockWidth = 300
groundHeight = 20
minBlockY = 250
maxBlockY = 400

resetBlocks :: State Game ()
resetBlocks = do
  blocks .= [firstBlock]
  nextBlockGap .= minBlockGap

updateBlocks :: State Game ()
updateBlocks = do
  s <- get
  blocks .= filter (\b -> rightSide b > 0) (s^.blocks)
  blocks %= map (\b -> b&hbX -~ s^.levelSpeed * s^.deltaTime)
  let rightmostSide = rightSide . HB.rightmost $ s^.blocks
  when (rightmostSide <= screenWidth - s^.nextBlockGap) $ do
    width <- randomRS minBlockWidth maxBlockWidth
    y <- randomRS minBlockY maxBlockY
    blocks <>= [Hitbox screenWidth y width groundHeight]
    assign nextBlockGap =<< randomRS minBlockGap maxBlockGap

drawBlock :: Hitbox -> IO ()
drawBlock Hitbox{..} = rect _hbX _hbY _hbWidth _hbHeight $ Pixel groundColor