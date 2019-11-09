module Constanten where

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

spH2 :: Float
spH2 = spH / 2

--Breedte van speler
spW :: Float
spW = 80

--Hoogte van Enemy1
e1H :: Float
e1H = 20

e1H2 :: Float
e1H2 = e1H / 2

e1W :: Float
e1W = 20

bH :: Float
bH = 8

bW :: Float
bW = 16

puntenVoorKill :: Float
puntenVoorKill = 10

bulletCounter :: Int
bulletCounter = 20 -- round $ 3 / nOSECSBETWEENCYCLES

immunityCounter :: Int
immunityCounter = round $ 1.5 / nOSECSBETWEENCYCLES

--Enemy spawn probability
eSpPr :: Float
eSpPr = 0.05

screenWidth :: Int
screenWidth = 1200

screenHeight :: Int
screenHeight = 800