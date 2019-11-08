module Enemy where

import Graphics.Gloss.Interface.IO.Game
import Constanten

data Enemy = Enemy { speedE :: Speed, positionE :: Position, healthE :: Health, sizeE :: Size}

type Speed = Float

type Position = (Float, Float)

type Health = Int

type Size = (Float, Float)