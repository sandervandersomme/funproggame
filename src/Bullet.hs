module Bullet where

import Graphics.Gloss.Interface.IO.Game

data Bullet = Bullet { speedB :: SpeedB, positionB :: PositionB, colourB :: ColourB}

type SpeedB = Float

type PositionB = (Float, Float)

type ColourB = Color