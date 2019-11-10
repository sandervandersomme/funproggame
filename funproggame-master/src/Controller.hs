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
    return $ bulletHitsEnemy $ collisionChecker $ newGsT ranNum gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

--Update iedere 'nOSECSBETWEENCYCLES' seconde
--Zorgt ervoor dat de bullets bewegen, dat de enemies bewegen.
--En bij ShowFinal zorgt het ervoor dat ook de speler blijft bewegen als w of s ingedrukt is.
newGsT :: Float -> GameState -> GameState
newGsT _ GameState{infoToShow = ShowGameState speler bullets, elapsedTime = _}
   = GameState (ShowGameState speler bullets) 0
newGsT randN GameState{infoToShow = ShowTest speler bullets enemies}
   | randN <= enemySpawnProb = GameState (ShowTest speler (updateBullets bullets) (updateEnemies enemies ++ 
     [Enemy 3 (600, berekenRandomY randN) 1 (enemy1Height,enemy1Width)]) ) 0
   | otherwise = GameState (ShowTest speler (updateBullets bullets) (updateEnemies enemies)) 0
{-
newGsT randN GameState{infoToShow = ShowFinal 'w' Player{speed=s, health=h, position=(x, y),
  bullCou=bc, immuCou=ic } bullets enemies enemyBullets score}
   | randN <= 0.1 = GameState (ShowFinal 'w' Player{speed=s,health=h,position=(x, min 400 (y+s)), 
     bullCou=bc, immuCou=ic } 
     (updateBullets bullets) ((updateEnemies enemies) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1 (enemy1Height,enemy1Width))]) enemyBullets score) 0
   | otherwise = GameState (ShowFinal 'w' Player{speed=s,health=h,position=(x, min 400 (y+s)),bullCou=bc,
     immuCou=ic} (updateBullets bullets) (updateEnemies enemies) enemyBullets score) 0
newGsT randN GameState{infoToShow = ShowFinal 's' Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bullets enemies enemyBullets score}
   | randN <= 0.1 = GameState (ShowFinal 's' Player{speed=s,health=h,position=(x, max (-400) y-s),
     bullCou=bc, immuCou=ic} 
     (updateBullets bullets) ((updateEnemies enemies) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1 (enemy1Height,enemy1Width))]) enemyBullets score) 0
   | otherwise = GameState (ShowFinal 's' Player{speed=s,health=h,position=(x, max (-400) y-s), 
     bullCou=bc, immuCou=ic} (updateBullets bullets) (updateEnemies enemies) enemyBullets score) 0
newGsT randN GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bullets enemies enemyBullets score}
   | randN <= 0.1 = GameState (ShowFinal c Player{speed=s,health=h,position=(x, y),bullCou=bc,immuCou=ic} 
     (updateBullets bullets) ((updateEnemies enemies) ++ [(Enemy 3 (600, (berekenRandomY randN)) 1 (enemy1Height,enemy1Width))]) enemyBullets score) 0
   | otherwise = GameState (ShowFinal c Player{speed=s,health=h,position=(x, y),bullCou=bc,immuCou=ic} 
     (updateBullets bullets) (updateEnemies enemies) enemyBullets score) 0
-}
newGsT randN GameState{infoToShow = ShowFinal c speler bullets enemies enemyBullets score}
  | randN <= enemySpawnProb = GameState (ShowFinal c (updatePlayer c speler) (updateBullets bullets) 
    (updateEnemies enemies ++ [Enemy enemyStartSpeed (600, berekenRandomY randN) 1 (enemy1Height,enemy1Width)]) enemyBullets score) 0
  | otherwise      = GameState (ShowFinal c (updatePlayer c speler) (updateBullets bullets) (updateEnemies enemies) enemyBullets score) 0
newGsT _ gs = gs

--random getal zorgt voor spreiding over y as. Dus 10% kans op nieuwe enemy betekent:
--(hoogte display/2 - hoogte enemy/2) / 0.05 = x
--Momenteel is dat 800/2 - 20/2 = 390 / 0.05 = 7800
berekenRandomY :: Float -> Float
berekenRandomY ran = ((fromIntegral screenHeight/2 - enemy1Height2) / (enemySpawnProb/2)) * (ran - (enemySpawnProb/2))

--collisionChecker moet kijken naar speler & enemy, speler & enemy bullets, speler bullets & enemies
collisionChecker :: GameState -> GameState
collisionChecker gs@GameState{infoToShow = ShowFinal c speler@Player{health=hp, immuCou=ic} bullets enemies enemyBullets score, elapsedTime = tt} 
    | fst check1 && ic == 0 && hp == 1 = GameState (ShowDead (updatePlayer 'h' speler) (snd check1) (updateScore score (-10))) tt
    | fst check1 && ic == 0 = GameState (ShowFinal c (updatePlayer 'h' speler) bullets (snd check1) enemyBullets (updateScore score (-10))) tt
    | fst check1 = GameState (ShowFinal c speler bullets (snd check1) enemyBullets (updateScore score (-10))) tt
    | otherwise = gs
       where check1 = enemyTouchesPlayer enemies speler []
collisionChecker gs = gs

updateScore :: Float -> Float -> Float
updateScore score points
  | score + points <= 0 = 0
  | otherwise = score + points

--Bekijkt of een Enemy en de speler hebben gebotst(collided).
enemyTouchesPlayer :: [Enemy] -> Player -> [Enemy] -> (Bool, [Enemy])
enemyTouchesPlayer [] _ enemiesDone = (False, enemiesDone)
enemyTouchesPlayer (e@Enemy{positionE = (xE,yE), sizeE=(eH,eW)}:enemies) speler@Player{health=h,position=(xP,yP)} enemiesDone 
  | inCollisionX xP playerWidth xE eW && abs (yP-yE) < (eH/2 + ((xP + abs xE) * (playerHeight2/playerWidth))) =
    (True, enemiesDone ++ enemies)
  | otherwise = enemyTouchesPlayer enemies speler (enemiesDone ++ [e])

--Bekijkt de collisions tussen de speler's bullets en de enemies. Hij past de bullets, enemies en score aan
--o.b.v. of er enemies gedood zijn.
bulletHitsEnemy :: GameState -> GameState
bulletHitsEnemy GameState{infoToShow = ShowFinal c speler bullets enemies enemyBullets score, elapsedTime = tt} =
  GameState (ShowFinal c speler (fst2 check) (snd2 check) enemyBullets (score + thrd check)) tt
    where check = collisionBullets bullets enemies [] 0
bulletHitsEnemy gs = gs

--Bekijkt voor iedere Bullet in [Bullet] of deze een Enemy heeft geraakt.
--Hij krijgt de [Enemy] (aangepast) terug. Deze is aangepast als een enemy geraakt is.
--Hij returnt [Bullet] (behalve de bullets die een enemy hebben geraakt), aangepaste [Enemy] en punten voor kills
collisionBullets :: [Bullet] -> [Enemy] -> [Bullet] -> Float -> ([Bullet], [Enemy], Float)
collisionBullets [] x bulletsDone score = (bulletsDone, x, score)
collisionBullets x [] bulletsDone score = (bulletsDone ++ x, [], score)
collisionBullets (b:bullets) enemies bulletsDone score
  | fst2 check = collisionBullets bullets (snd2 check) (bulletsDone ++ [b]) score
  | otherwise = collisionBullets bullets (snd2 check) bulletsDone (thrd check)
     where check = bulletMisses b enemies []

--Bekijkt voor 1 Bullet of hij collision vrij is. (dus geen enemy heeft geraakt)
--Ja --> return (True, alle enemies, 0)
--Nee -> return (False, alle enemies (met 1 enemy die geraakt en aangepast is), puntenVoorKill)
bulletMisses :: Bullet -> [Enemy] -> [Enemy] -> (Bool, [Enemy], Float)
bulletMisses _ [] enemiesDone = (True, enemiesDone, 0)
bulletMisses b@Bullet{positionB = (xB, yB)} (e@Enemy{positionE = (xE, yE), sizeE = (eH, eW)}:enemies) enemiesDone
  | inCollisionX xB bulletWidth xE eW && inCollisionY yB bulletHeight yE eH = (False, enemiesDone ++ fst check ++ enemies, snd check)
  | otherwise = bulletMisses b enemies (enemiesDone ++ [e])
    where check = enemyHit e

--Berekent nieuwe health voor Enemy die geraakt is door een kogel. Als het 0 is geworden is de enemy dood
enemyHit :: Enemy -> ([Enemy], Float)
enemyHit e@Enemy{speedE = s, positionE = p, healthE = h, sizeE = g} 
  | h-1 > 0   = ([Enemy s p (h-1) g], 0)
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
updateBullets (Bullet{speedB=s,positionB=(x,y),colourB=c}:bullets) 
  | (x - bulletWidth) < 600   = Bullet s (x+s,y) c : updateBullets bullets
  | otherwise        = updateBullets bullets

--Update de locatie van de Enemies. Iedere enemy gaat met s (speed) naar links
updateEnemies :: [Enemy] -> [Enemy]
updateEnemies [] = []
updateEnemies (Enemy{speedE=s,positionE=(x,y),healthE=h,sizeE=(eH,eW)}:enemies) 
  | (x + eW) > -600 = Enemy s (x-s,y) h (eH,eW) : updateEnemies enemies
  | otherwise       = updateEnemies enemies

--Update de speler als w/s ingedrukt is, maar ook als de speler geraakt is.
updatePlayer :: Char -> Player -> Player
updatePlayer 'w' Player{speed=s, health=h, position=(x,y), bullCou=bc, immuCou=ic} 
  = Player s h (x, min 400 (y+s)) (max 0 (bc-1)) (max 0 (ic-1))
updatePlayer 's' Player{speed=s, health=h, position=(x,y), bullCou=bc, immuCou=ic}
  = Player s h (x, max (-400) (y-s)) (max 0 (bc-1)) (max 0 (ic-1))
updatePlayer 'h' Player{speed=s, health=h, position=p, bullCou=bc, immuCou=ic}
  = Player s (h-1) p bc immunityCounter
updatePlayer _ Player{speed=s, health=h, position=(x,y), bullCou=bc, immuCou=ic}
  = Player s h (x, y) (max 0 (bc-1)) (max 0 (ic-1))

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) pr _ _) gstate
  -- If the user presses a character key, show that one
   | c == 'n' =
       GameState (ShowGameState (Player 0 3 (-350, 0) 0 0) []) 0
   | c == 'p' = gstate { infoToShow = ShowPause }
   | c == 'h' = gstate { infoToShow = ShowHighScores }
   -- pr == Down zodat hij alleen beweegt als de key ingedrukt wordt, en niet ook als de key wordt losgelaten
   | c == 'w' && pr == Up   = verwijderC c gstate
   | c == 'w' && pr == Down = vgsW gstate
   | c == 's' && pr == Up   = verwijderC c gstate
   | c == 's' && pr == Down = vgsS gstate
   | c == 'z' && pr == Down = vgsTest gstate
   | c == 'q' && pr == Down = gstate { infoToShow = ShowTriangle (Player 0 3 (-350, 0) 0 0)}
   | c == 'u' && pr == Down = gstate { infoToShow = ShowFinal '_' (Player 5 playerStartHealth (playerStartX, playerStartY) 0 0) [] [] [] 0}
   | otherwise = gstate
--   = gstate { infoToShow = ShowPause }
inputKey _ gstate = gstate -- Otherwise keep the same

--Als de w-key of s-key wordt losgelaten dan moet de GameState geupdate worden.
--Dit verandert alleen de c variabele in ShowFinal
verwijderC :: Char -> GameState -> GameState
verwijderC 'w' GameState{infoToShow = ShowFinal c speler bullets enemies enemyBullets score, elapsedTime = tt}
  | c == '+'  = GameState{infoToShow = ShowFinal 's' speler bullets enemies enemyBullets score, elapsedTime = tt}
  | otherwise = GameState{infoToShow = ShowFinal '_' speler bullets enemies enemyBullets score, elapsedTime = tt}
verwijderC 's' GameState{infoToShow = ShowFinal c speler bullets enemies enemyBullets score, elapsedTime = tt}
  | c == '+'  = GameState{infoToShow = ShowFinal 'w' speler bullets enemies enemyBullets score, elapsedTime = tt}
  | otherwise = GameState{infoToShow = ShowFinal '_' speler bullets enemies enemyBullets score, elapsedTime = tt}
verwijderC _ gs = gs

--Als de w-key wordt ingedrukt
--De speler gaat omhoog. Bij ShowFinal gaat hij niet omhoog als s nog ingedrukt is
vgsW :: GameState -> GameState
-- min 400 y+10 zodat hij niet buiten het veld gaat
vgsW GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y), bullCou=bc, immuCou=ic}
   bullets, elapsedTime = tt}
   = GameState (ShowGameState (Player s h (-400, min 400 y+10) bc ic) bullets) tt
vgsW GameState{infoToShow = ShowTest  Player{speed=s, health=h, position=(x, y), bullCou=bc, immuCou=ic} 
  bullets enemies, elapsedTime = tt}
   = GameState (ShowTest (Player s h (-400, min 400 y+10) bc ic) bullets enemies) tt
vgsW GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y), bullCou=bc, immuCou=ic} 
  bullets enemies enemyBullets score, elapsedTime = tt}
   | c == '_'  = GameState (ShowFinal 'w' (Player s h (x, y) bc ic) bullets enemies enemyBullets score) tt
   | c == 's'  = GameState (ShowFinal '+' (Player s h (x, y) bc ic) bullets enemies enemyBullets score) tt
   | otherwise = GameState (ShowFinal '_' (Player s h (x, y) bc ic) bullets enemies enemyBullets score) tt
vgsW gs = gs

--Als de s-key wordt ingedrukt
--De speler gaat omlaag. Bij ShowFinal gaat hij niet omlaag als w nog ingedrukt is
vgsS :: GameState -> GameState
vgsS GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bullets, elapsedTime = tt}
   = GameState (ShowGameState (Player s h (-400, max (-400) y-10) bc ic) bullets) tt
vgsS GameState{infoToShow = ShowTest Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bullets enemies, elapsedTime = tt}
   = GameState (ShowTest (Player s h (-400, max (-400) y-10) bc ic) bullets enemies) tt
vgsS GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bullets enemies enemyBullets score, elapsedTime = tt}
   | c == '_'  = GameState (ShowFinal 's' (Player s h (x, y) bc ic) bullets enemies enemyBullets score) tt
   | c == 'w'  = GameState (ShowFinal '+' (Player s h (x, y) bc ic) bullets enemies enemyBullets score) tt
   | otherwise = GameState (ShowFinal '_' (Player s h (x, y) bc ic) bullets enemies enemyBullets score) tt
vgsS gs = gs

--Als een kogel wordt afgevuurd (door de speler)
--wordt een kogel toegevoegd aan bullets(bullets = [Bullet])
vgsTest :: GameState -> GameState
vgsTest GameState{infoToShow = ShowGameState speler@Player{speed=s, position=(x, y)} bullets, elapsedTime = tt}
   -- = GameState (ShowTest (Player s h (x, y)) [(Bullet 5 (x + 10, y) blue)] []) tt
   = GameState (ShowTest speler [Bullet 5 (x + 10, y) blue] []) tt
vgsTest GameState{infoToShow = ShowTest speler@Player{speed=s, position=(x, y)} bullets enemies, elapsedTime = tt}
   = GameState (ShowTest speler (bullets ++ [Bullet 5 (x + 10, y) blue]) enemies) tt
vgsTest gs@GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bullets enemies enemyBullets score, elapsedTime = tt}
    -- = GameState (ShowFinal c (Player s h (x, y) bulletCounter ic) (bullets ++ [(Bullet 10 (x + 10, y) blue)]) enemies enemyBullets score) tt
    | bc < 1    = GameState (ShowFinal c (Player s h (x, y) bulletCounter ic) 
      (bullets ++ [Bullet 10 (x + 10, y) blue]) enemies enemyBullets score) tt
    | otherwise = gs
vgsTest gs = gs