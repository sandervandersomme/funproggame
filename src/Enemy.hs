module Enemy where

import Graphics.Gloss.Interface.IO.Game

data Enemy = Enemy { speedE :: Speed, positionE :: Position, healthE :: Health}

type Speed = Float

type Position = (Float, Float)

type Health = Int