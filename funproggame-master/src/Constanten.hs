module Constanten where

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 0.025
--WAS EERST 5, NU 2 OM TE TESTEN.

--Start X voor speler
playerStartX :: Float
playerStartX = -400

--Start Y voor speler
playerStartY :: Float
playerStartY = 0

--Start Healt voor speler
playerStartHealth :: Int
playerStartHealth = 3

--Hoogte van speler
playerHeight :: Float
playerHeight = 60

playerHeight2 :: Float
playerHeight2 = playerHeight / 2

--Breedte van speler
playerWidth :: Float
playerWidth = 80

--Hoogte van Enemy1
enemy1Height :: Float
enemy1Height = 20

enemy1Height2 :: Float
enemy1Height2 = enemy1Height / 2

enemy1Width :: Float
enemy1Width = 20

bulletHeight :: Float
bulletHeight = 8

bulletWidth :: Float
bulletWidth = 16

puntenVoorKill :: Float
puntenVoorKill = 10

enemyStartSpeed :: Float
enemyStartSpeed = 8 --3

bulletCounter :: Int
bulletCounter = 20 -- round $ 3 / nOSECSBETWEENCYCLES

immunityCounter :: Int
immunityCounter = round $ 1.5 / nOSECSBETWEENCYCLES

--Enemy spawn probability
enemySpawnProb :: Float
enemySpawnProb = 0.05

screenWidth :: Int
screenWidth = 1200

screenHeight :: Int
screenHeight = 800

amountOfHighscores :: Int
amountOfHighscores = 5