-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Player
import Bullet
import Enemy

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES
  =
  do
    ranNum <- randomIO :: IO Float
    --return $ newGstate gstate
    return $ newGsT ranNum gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

--Update iedere 'nOSECSBETWEENCYCLES' seconde
--Zorgt ervoor dat de bullets bewegen, dat de enemies bewegen.
--En bij ShowFinal zorgt het ervoor dat ook de speler blijft bewegen als w of s ingedrukt is.
newGsT :: Float -> GameState -> GameState
newGsT _ GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = _}
   = GameState (ShowGameState (Player s h (x, y)) bs) 0
newGsT _ GameState{infoToShow = ShowBullets [Bullet{speedB=s,positionB=(x,y),colourB=c}]}
   = GameState (ShowBullets [b]) 0
     where b = (Bullet s (x+s, y) c)
newGsT randN GameState{infoToShow = ShowTest speler bs es}
   | randN <= 0.1 = GameState (ShowTest speler (updateBullets bs) ((updateEnemies es) ++ 
     [(Enemy 3 (600, (berekenRandomY randN)) 1)]) ) 0
   | otherwise = GameState (ShowTest speler (updateBullets bs) (updateEnemies es)) 0
newGsT randN GameState{infoToShow = ShowFinal 'w' Player{speed=s, health=h, position=(x, y)} bs es ebs sc}
   | randN <= 0.1 = GameState (ShowFinal 'w' Player{speed=s,health=h,position=(x, y+s)} (updateBullets bs) 
     ((updateEnemies es) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1)]) ebs sc) 0
   | otherwise = GameState (ShowFinal 'w' Player{speed=s,health=h,position=(x, y+s)} (updateBullets bs) 
     (updateEnemies es) ebs sc) 0
newGsT randN GameState{infoToShow = ShowFinal 's' Player{speed=s, health=h, position=(x, y)} bs es ebs sc}
   | randN <= 0.1 = GameState (ShowFinal 's' Player{speed=s,health=h,position=(x, y-s)} (updateBullets bs) 
     ((updateEnemies es) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1)]) ebs sc) 0
   | otherwise = GameState (ShowFinal 's' Player{speed=s,health=h,position=(x, y-s)} (updateBullets bs) 
     (updateEnemies es) ebs sc) 0
newGsT randN GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y)} bs es ebs sc}
   | randN <= 0.1 = GameState (ShowFinal c Player{speed=s,health=h,position=(x, y)} (updateBullets bs) 
     ((updateEnemies es) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1)]) ebs sc) 0
   | otherwise = GameState (ShowFinal c Player{speed=s,health=h,position=(x, y)} (updateBullets bs) 
     (updateEnemies es) ebs sc) 0
newGsT _ gs = gs

--random getal zorgt voor spreiding over y as. Dus 10% kans op nieuwe enemy betekent:
--(hoogte display/2 - hoogte enemy/2) / 0.05 = x
--Momenteel is dat 800/2 - 20/2 = 390 / 0.05 = 7800
berekenRandomY :: Float -> Float
berekenRandomY ran = 7800 * (ran - 0.05)

{-OUDE FUNCTIE
newGstate :: GameState -> GameState
newGstate GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = _}
   = GameState (ShowGameState (Player s h (x, y)) bs) 0
newGstate GameState{infoToShow = ShowBullets [Bullet{speedB=s,positionB=(x,y),colourB=c}]}
   = GameState (ShowBullets [b]) 0
     where b = (Bullet s (x+s, y) c)
newGstate GameState{infoToShow = ShowTest speler bs es}
   = GameState (ShowTest speler (updateBullets bs) (updateEnemies es)) 0
newGstate gs = gs
-}

--Update de locatie van de bullets. Iedere bullet gaat met s (speed) naar rechts
updateBullets :: [Bullet] -> [Bullet]
updateBullets [] = []
updateBullets (Bullet{speedB=s,positionB=(x,y),colourB=c}:bs) = 
  (Bullet s (x+s,y) c) : updateBullets bs

--Update de locatie van de Enemies. Iedere enemy gaat met s (speed) naar links
updateEnemies :: [Enemy] -> [Enemy]
updateEnemies [] = []
updateEnemies (Enemy{speedE=s,positionE=(x,y),healthE=h}:es) =
  (Enemy s (x-s,y) h) : updateEnemies es

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) pr _ _) gstate
  -- If the user presses a character key, show that one
   | c == 'n' =
       GameState (ShowGameState (Player 0 3 (-350, 0)) []) 0
   | c == 'p' = gstate { infoToShow = ShowPause }
   -- pr == Down zodat hij alleen beweegt als de key ingedrukt wordt, en niet ook als de key wordt losgelaten
   | c == 'w' && pr == Up   = verwijderC c gstate
   | c == 'w' && pr == Down = vgsW gstate
   | c == 's' && pr == Up   = verwijderC c gstate
   | c == 's' && pr == Down = vgsS gstate
   | c == 'b' && pr == Down = gstate { infoToShow = ShowBullets [(Bullet 10 (-350, 0) red)]}
   | c == 'z' && pr == Down = vgsTest gstate
   | c == 'q' && pr == Down = gstate { infoToShow = ShowTriangle (Player 0 3 (-350, 0))}
   | c == 'u' && pr == Down = gstate { infoToShow = ShowFinal '_' (Player 5 3 (-400, 0)) [] [] [] 0}
   | otherwise = gstate
--   = gstate { infoToShow = ShowPause }
inputKey _ gstate = gstate -- Otherwise keep the same

--Als de w-key of s-key wordt losgelaten dan moet de GameState geupdate worden.
--Dit verandert alleen de c variabele in ShowFinal
verwijderC :: Char -> GameState -> GameState
verwijderC 'w' GameState{infoToShow = ShowFinal c speler bs es ebs sc, elapsedTime = tt}
  | c == '+'  = GameState{infoToShow = ShowFinal 's' speler bs es ebs sc, elapsedTime = tt}
  | otherwise = GameState{infoToShow = ShowFinal '_' speler bs es ebs sc, elapsedTime = tt}
verwijderC 's' GameState{infoToShow = ShowFinal c speler bs es ebs sc, elapsedTime = tt}
  | c == '+'  = GameState{infoToShow = ShowFinal 'w' speler bs es ebs sc, elapsedTime = tt}
  | otherwise = GameState{infoToShow = ShowFinal '_' speler bs es ebs sc, elapsedTime = tt}
verwijderC _ gs = gs

--Als de w-key wordt ingedrukt
--De speler gaat omhoog. Bij ShowFinal gaat hij niet omhoog als s nog ingedrukt is
vgsW :: GameState -> GameState
-- min 400 y+10 zodat hij niet buiten het veld gaat
vgsW GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = tt}
   = GameState (ShowGameState (Player s h (-400, (min 400 y+10))) bs) tt
vgsW GameState{infoToShow = ShowTest Player{speed=s, health=h, position=(x, y)} bs es, elapsedTime = tt}
   = GameState (ShowTest (Player s h (-400, (min 400 y+10))) bs es) tt
vgsW GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y)} bs es ebs sc, elapsedTime = tt}
   | c == '_'  = GameState (ShowFinal 'w' (Player s h (x, (min 400 y+s))) bs es ebs sc) tt
   | c == 's'  = GameState (ShowFinal '+' (Player s h (x, y)) bs es ebs sc) tt
   | otherwise = GameState (ShowFinal '_' (Player s h (x, y)) bs es ebs sc) tt
vgsW gs = gs

--Als de s-key wordt ingedrukt
--De speler gaat omlaag. Bij ShowFinal gaat hij niet omlaag als w nog ingedrukt is
vgsS :: GameState -> GameState
vgsS GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = tt}
   = GameState (ShowGameState (Player s h (-400, (max (-400) y-10))) bs) tt
vgsS GameState{infoToShow = ShowTest Player{speed=s, health=h, position=(x, y)} bs es, elapsedTime = tt}
   = GameState (ShowTest (Player s h (-400, (max (-400) y-10))) bs es) tt
vgsS GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y)} bs es ebs sc, elapsedTime = tt}
   | c == '_'  = GameState (ShowFinal 's' (Player s h (x, (min 400 y-s))) bs es ebs sc) tt
   | c == 'w'  = GameState (ShowFinal '+' (Player s h (x, y)) bs es ebs sc) tt
   | otherwise = GameState (ShowFinal '_' (Player s h (x, y)) bs es ebs sc) tt
vgsS gs = gs

--Als een kogel wordt afgevuurd (door de speler)
--wordt een kogel toegevoegd aan bs(bs = [Bullet])
vgsTest :: GameState -> GameState
vgsTest GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = tt}
   = GameState (ShowTest (Player s h (x, y)) [(Bullet 5 (x + 10, y) blue)] []) tt
vgsTest GameState{infoToShow = ShowTest Player{speed=s, health=h, position=(x, y)} bs es, elapsedTime = tt}
   = GameState (ShowTest (Player s h (x, y)) (bs ++ [(Bullet 5 (x + 10, y) blue)]) es) tt
vgsTest GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y)} bs es ebs sc, elapsedTime = tt}
   = GameState (ShowFinal c (Player s h (x, y)) (bs ++ [(Bullet 5 (x + 10, y) blue)]) es ebs sc) tt
vgsTest gs = gs