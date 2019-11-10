module Constanten where

import Graphics.Gloss

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 0.025
--WAS EERST 5, NU 2 OM TE TESTEN.

--Start X voor speler
spX :: Float
spX = -400

--Start Y voor speler
spY :: Float
spY = 0

--Hoogte van speler
spH :: Float
spH = 60

--Breedte van speler
spW :: Float
spW = 80

--Hoogte van Enemy1
e1H :: Float
e1H = 20

e1W :: Float
e1W = 20

e2H :: Float
e2H = 80

e2W :: Float
e2W = 80

e1S :: Float
e1S = 5

e2S :: Float
e2S = 3

e1Health :: Int
e1Health = 1

e2Health :: Int
e2Health = 2

bH :: Float
bH = 8

bW :: Float
bW = 16

puntenVoorKill :: Float
puntenVoorKill = 10

bulletCounter :: Int
bulletCounter = 16 

bulletCounter1v1 :: Int
bulletCounter1v1 = 12 

bulletCounterEnemy :: Int
bulletCounterEnemy = 20 

immunityCounter :: Int
immunityCounter = round $ 1.5 / nOSECSBETWEENCYCLES

--Enemy spawn probability
eSpPr :: Float
eSpPr = 0.05

--Spawn probability van Enemy1
e1SpPr :: Float
e1SpPr = 0.8

e2SpPr :: Float
e2SpPr = 0.08

screenWidth :: Int
screenWidth = 1200

screenHeight :: Int
screenHeight = 800

bullS :: Float
bullS = 10

--Enemy spawn X
eSpX :: Float
eSpX = 600

eDamage :: Int -> Int
eDamage 1 = 1
eDamage 2 = 1 

amountOfHighscores :: Int
amountOfHighscores = 5