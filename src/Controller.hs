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
import Constanten

fst2 :: (a, b, c) -> a
fst2 (x,_,_) = x

snd2 :: (a, b, c) -> b
snd2 (_,x,_) = x

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES
  =
  do
    ranNum <- randomIO :: IO Float
    --return $ newGstate gstate
    return $ collBulEnem $ collisionChecker $ newGsT ranNum gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

--Update iedere 'nOSECSBETWEENCYCLES' seconde
--Zorgt ervoor dat de bullets bewegen, dat de enemies bewegen.
--En bij ShowFinal zorgt het ervoor dat ook de speler blijft bewegen als w of s ingedrukt is.
newGsT :: Float -> GameState -> GameState
newGsT _ GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = _}
   = GameState (ShowGameState (Player s h (x, y)) bs) 0
newGsT randN GameState{infoToShow = ShowTest speler bs es}
   | randN <= 0.1 = GameState (ShowTest speler (updateBullets bs) ((updateEnemies es) ++ 
     [(Enemy 3 (600, (berekenRandomY randN)) 1 (e1H,e1W))]) ) 0
   | otherwise = GameState (ShowTest speler (updateBullets bs) (updateEnemies es)) 0
newGsT randN GameState{infoToShow = ShowFinal 'w' Player{speed=s, health=h, position=(x, y)} bs es ebs sc}
   | randN <= 0.1 = GameState (ShowFinal 'w' Player{speed=s,health=h,position=(x, min 400 (y+s))} 
     (updateBullets bs) ((updateEnemies es) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1 (e1H,e1W))]) ebs sc) 0
   | otherwise = GameState (ShowFinal 'w' Player{speed=s,health=h,position=(x, min 400 (y+s))} (updateBullets bs) 
     (updateEnemies es) ebs sc) 0
newGsT randN GameState{infoToShow = ShowFinal 's' Player{speed=s, health=h, position=(x, y)} bs es ebs sc}
   | randN <= 0.1 = GameState (ShowFinal 's' Player{speed=s,health=h,position=(x, max (-400) y-s)} 
     (updateBullets bs) ((updateEnemies es) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1 (e1H,e1W))]) ebs sc) 0
   | otherwise = GameState (ShowFinal 's' Player{speed=s,health=h,position=(x, max (-400) y-s)} (updateBullets bs) 
     (updateEnemies es) ebs sc) 0
newGsT randN GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y)} bs es ebs sc}
   | randN <= 0.1 = GameState (ShowFinal c Player{speed=s,health=h,position=(x, y)} (updateBullets bs) 
     ((updateEnemies es) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1 (e1H,e1W))]) ebs sc) 0
   | otherwise = GameState (ShowFinal c Player{speed=s,health=h,position=(x, y)} (updateBullets bs) 
     (updateEnemies es) ebs sc) 0
newGsT _ gs = gs

--random getal zorgt voor spreiding over y as. Dus 10% kans op nieuwe enemy betekent:
--(hoogte display/2 - hoogte enemy/2) / 0.05 = x
--Momenteel is dat 800/2 - 20/2 = 390 / 0.05 = 7800
berekenRandomY :: Float -> Float
berekenRandomY ran = 7800 * (ran - 0.05)

--collisionChecker moet kijken naar speler & enemy, speler & enemy bullets, speler bullets & enemies
collisionChecker :: GameState -> GameState
collisionChecker gs@GameState{infoToShow = ShowFinal c sp@Player{speed=s,health=h,position=(x,y)} 
  bs es ebs sc, elapsedTime = tt} 
    | fst check1 == True = GameState (ShowFinal c sp bs (snd check1) ebs (sc-10) ) tt
    | otherwise = gs
       where check1 = collisionCheckEs es sp []
collisionChecker gs = gs

--Bekijkt of een Enemy en de speler hebben gebotst(collided).
collisionCheckEs :: [Enemy] -> Player -> [Enemy] -> (Bool, [Enemy])
collisionCheckEs [] _ esDone = (False, esDone)
collisionCheckEs (e@Enemy{positionE = (xE,yE), sizeE=(eH,eW)}:es) sp@Player{health=h,position=(xP,yP)} esDone 
  | inCollisionX xP spW xE eW && (abs (yP-yE)) < (eH/2 + ((xP + abs xE) * (spH2/spW))) =
    (True, (esDone ++ es))
  | otherwise = collisionCheckEs es sp (esDone ++ [e])

--Bekijkt de collisions tussen de speler's bullets en de enemies. Hij past de bullets, enemies en score aan
--o.b.v. of er enemies gedood zijn.
collBulEnem :: GameState -> GameState
collBulEnem GameState{infoToShow = ShowFinal c sp bs es ebs sc, elapsedTime = tt} =
  GameState (ShowFinal c sp (fst2 check) (snd2 check) ebs (sc + thrd check)) tt
    where check = collisionBullets bs es [] 0
collBulEnem gs = gs

--Bekijkt voor iedere Bullet in [Bullet] of deze een Enemy heeft geraakt.
--Hij krijgt de [Enemy] (aangepast) terug. Deze is aangepast als een enemy geraakt is.
--Hij returnt [Bullet] (behalve de bullets die een enemy hebben geraakt), aangepaste [Enemy] en punten voor kills
collisionBullets :: [Bullet] -> [Enemy] -> [Bullet] -> Float -> ([Bullet], [Enemy], Float)
collisionBullets [] x bsDone sc = (bsDone, x, sc)
collisionBullets x [] bsDone sc = ((bsDone ++ x), [], sc)
collisionBullets (b:bs) es bsDone sc
  | fst2 check == True = collisionBullets bs (snd2 check) (bsDone ++ [b]) sc
  | otherwise = collisionBullets bs (snd2 check) bsDone (thrd check)
     where check = collisionFree b es []

--Bekijkt voor 1 Bullet of hij collision vrij is. (dus geen enemy heeft geraakt)
--Ja --> return (True, alle enemies, 0)
--Nee -> return (False, alle enemies (met 1 enemy die geraakt en aangepast is), puntenVoorKill)
collisionFree :: Bullet -> [Enemy] -> [Enemy] -> (Bool, [Enemy], Float)
collisionFree _ [] esDone = (True, esDone, 0)
collisionFree b@Bullet{positionB = (xB, yB)} (e@Enemy{positionE = (xE, yE), sizeE = (eH, eW)}:es) esDone
  | (inCollisionX xB bW xE eW) && (inCollisionY yB bH yE eH) = (False, esDone ++ (fst check) ++ es, snd check)
  | otherwise = collisionFree b es (esDone ++ [e])
    where check = enemyHit e

--Berekent nieuwe health voor Enemy die geraakt is door een kogel. Als het 0 is geworden is de enemy dood
enemyHit :: Enemy -> ([Enemy], Float)
enemyHit e@Enemy{speedE = s, positionE = p, healthE = h, sizeE = g} 
  | h-1 > 0   = ([(Enemy s p (h-1) g)], 0)
  | otherwise = ([], puntenVoorKill) 

--Bekijkt of de x-waardes van twee objecten zo zijn dat ze in een botsing kunnen zitten.
--speler of zijn bullet = 1: x1  y1      width1 heigth1     
inCollisionX :: Float -> Float -> Float -> Float -> Bool
inCollisionX x1 w1 x2 w2 | x2 < x1 && x2 + w2 > x1 - w1 = True
                         | otherwise                    = False

--spelers bullet = 1
inCollisionY :: Float -> Float -> Float -> Float -> Bool
--onderkant van 2 is lager dan bovenkant 1
inCollisionY y1 h1 y2 h2 | abs (y1 - y2) < (h1/2 + h2/2) = True
                         | otherwise                     = False

--Update de locatie van de bullets. Iedere bullet gaat met s (speed) naar rechts
updateBullets :: [Bullet] -> [Bullet]
updateBullets [] = []
updateBullets (Bullet{speedB=s,positionB=(x,y),colourB=c}:bs) 
  | (x - bW) < 600   = (Bullet s (x+s,y) c) : updateBullets bs
  | otherwise        = updateBullets bs

--Update de locatie van de Enemies. Iedere enemy gaat met s (speed) naar links
updateEnemies :: [Enemy] -> [Enemy]
updateEnemies [] = []
updateEnemies (Enemy{speedE=s,positionE=(x,y),healthE=h,sizeE=(eH,eW)}:es) 
  | (x + eW) > -600 = (Enemy s (x-s,y) h (eH,eW)) : updateEnemies es
  | otherwise       = updateEnemies es

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
   | c == 'z' && pr == Down = vgsTest gstate
   | c == 'q' && pr == Down = gstate { infoToShow = ShowTriangle (Player 0 3 (-350, 0))}
   | c == 'u' && pr == Down = gstate { infoToShow = ShowFinal '_' (Player 5 3 (spX, spY)) [] [] [] 0}
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
   | c == '_'  = GameState (ShowFinal 'w' (Player s h (x, y)) bs es ebs sc) tt
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
   | c == '_'  = GameState (ShowFinal 's' (Player s h (x, y)) bs es ebs sc) tt
   | c == 'w'  = GameState (ShowFinal '+' (Player s h (x, y)) bs es ebs sc) tt
   | otherwise = GameState (ShowFinal '_' (Player s h (x, y)) bs es ebs sc) tt
vgsS gs = gs

--Als een kogel wordt afgevuurd (door de speler)
--wordt een kogel toegevoegd aan bs(bs = [Bullet])
vgsTest :: GameState -> GameState
vgsTest GameState{infoToShow = ShowGameState sp@Player{speed=s, position=(x, y)} bs, elapsedTime = tt}
   -- = GameState (ShowTest (Player s h (x, y)) [(Bullet 5 (x + 10, y) blue)] []) tt
   = GameState (ShowTest sp [(Bullet 5 (x + 10, y) blue)] []) tt
vgsTest GameState{infoToShow = ShowTest Player{speed=s, health=h, position=(x, y)} bs es, elapsedTime = tt}
   = GameState (ShowTest (Player s h (x, y)) (bs ++ [(Bullet 5 (x + 10, y) blue)]) es) tt
vgsTest GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y)} bs es ebs sc, elapsedTime = tt}
   = GameState (ShowFinal c (Player s h (x, y)) (bs ++ [(Bullet 10 (x + 10, y) blue)]) es ebs sc) tt
vgsTest gs = gs