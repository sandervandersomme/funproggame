module Enemy where

import Graphics.Gloss.Interface.IO.Game
import Constanten

data Enemy = Enemy { speedE :: Speed, positionE :: Position, healthE :: Health, sizeE :: Size, 
  typeE :: TypeEnemy, colour :: Colour, bullC :: BulletCounter}

type Speed = Float

type Position = (Float, Float)

type Health = Int

type Size = (Float, Float)

type TypeEnemy = Int

type Colour = Color

type BulletCounter = Int

getColour :: Float -> Color
getColour (-3) = azure
getColour (-1) = orange
getColour 1    = white
getColour 2    = greyN 0.5
getColour _    = green

getE :: Int -> Enemy
getE 1 = Enemy e1S (eSpX,0) e1Health (e1H,e1W) 1 (getColour 1) 0
getE 2 = Enemy e2S (eSpX,0) e2Health (e2H,e2W) 2 (getColour 2) 0
getE 3 = Enemy (e1S*2) (eSpX,0) e1Health (e1H,e1W) 1 (getColour 1) 0