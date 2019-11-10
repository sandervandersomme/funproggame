module Collisions where

import Constanten
import Player
import Enemy
import Bullet
import Model
import HulpFuncties
import Updates

--Berekent nieuwe health voor Enemy die geraakt is door een kogel. Als het 0 is geworden is de enemy dood
enemyHit :: Enemy -> ([Enemy], Float)
enemyHit e@Enemy{speedE = s, positionE = p, healthE = h, sizeE = g, typeE=t, colour=co, bullC=bc} 
  | h-1 > 0   = ([(Enemy s p (h-1) g t co bc)], 0)
  | otherwise = ([], puntenVoorKill) 

--Of een Player/(of zijn bullet) botst met een Enemy/bullet
inCollisionP :: Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
inCollisionP 2 x1 w1 x2 w2 y1 h1 y2 h2 
  | x2 > x1 && x2 - w2 < x1 + w1 && 
    (abs (y1-y2)) < (h2/2 + ((x2 - x1) * ((h1/2)/w1))) = True
  | otherwise = False
inCollisionP 1 x1 w1 x2 w2 y1 h1 y2 h2 
  | x2 < x1 && x2 + w2 > x1 - w1 && (abs (y1-y2)) < (h2/2 + ((x1 + abs x2) * ((h1/2)/w1))) = True
  | otherwise = False
--1 voor bullet, 2 voor enemy
inCollisionP (-1) x1 w1 x2 w2 y1 h1 y2 h2 
  | x2 < x1 && x2 + w2 > x1 - w1 && (abs (y1-y2)) < (h2/2 + h1/2) = True
  | otherwise = False
inCollisionP (-2) x1 w1 x2 w2 y1 h1 y2 h2 
  | x1 < x2 && x2 - w2 < x1 + w1 && (abs (y1-y2)) < (h2/2 + h1/2) = True
  | otherwise = False
inCollisionP _ _ _ _ _ _ _ _ _ = False

--Deel 2
--
--
--
--
--

--collisionChecker moet kijken naar speler & enemy, speler & enemy bullets, speler bullets & enemies
collisionChecker :: GameState -> GameState
collisionChecker gs@GameState{infoToShow = ShowFinal c sp@Player{health=h,immuCou=ic} bs es ebs sc, elapsedTime = tt} 
    | fst check1 == True && ic == 0 && h == 1 
      = GameState (ShowDead (updatePlayer 'h' sp) (snd check1) (updateScore sc (-10)) "_") tt
    | fst check1 == True && ic == 0 = GameState (ShowFinal c (updatePlayer 'h' sp) 
      bs (snd check1) ebs (updateScore sc (-10)) ) tt
    | fst check1 == True = GameState (ShowFinal c sp bs (snd check1) ebs (updateScore sc (-10))) tt
    | otherwise = gs
       where check1 = collisionCheckEs es sp []
collisionChecker gs@GameState{infoToShow=Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt}
  = GameState (Show1v1 c1 (fst check2) c2 (fst check3) (snd check3) (snd check2) es1 es2) tt
    where check2 = collisionCheckEnemyBullets 1 sp1 bs2 [] 
          check3 = collisionCheckEnemyBullets 2 sp2 bs1 []
collisionChecker gs@GameState{infoToShow = ShowLevel c sp@Player{immuCou=ic} bs es ebs sc t l, elapsedTime = tt} 
    | fst check1 == True && ic == 0 = GameState (ShowLevel c (updatePlayer 'h' sp) 
      bs (snd check1) ebs (sc-10) t l) tt
    | fst check1 == True = GameState (ShowLevel c sp bs (snd check1) ebs sc t l) tt
    | otherwise = gs
       where check1 = collisionCheckEs es sp []
collisionChecker gs = gs

collisionChecker2 :: GameState -> GameState
collisionChecker2 gs@GameState{infoToShow=Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt}
  = GameState (Show1v1 c1 (fst check2) c2 (fst check3) bs1 bs2 (snd check2) (snd check3)) tt
    where check2 = collisionCheckEnemy 1 sp1 es1 [] 
          check3 = collisionCheckEnemy 2 sp2 es2 []
collisionChecker2 gs = gs

--Bekijkt of een Enemy en de speler hebben gebotst(collided).
collisionCheckEs :: [Enemy] -> Player -> [Enemy] -> (Bool, [Enemy])
collisionCheckEs [] _ esDone = (False, esDone)
collisionCheckEs (e@Enemy{positionE = (xE,yE), sizeE=(eH,eW)}:es) sp@Player{health=h,position=(xP,yP)} esDone 
  | (inCollisionP 1 xP spW xE eW yP spH yE eH) == True
    = (True, (esDone ++ es))
  | otherwise = collisionCheckEs es sp (esDone ++ [e])

collisionCheckEnemyBullets :: Int -> Player -> [Bullet] -> [Bullet] -> (Player, [Bullet])
collisionCheckEnemyBullets i sp [] x = (sp, x)
collisionCheckEnemyBullets i sp@Player{health=h,position=(xP,yP)} (b@Bullet{positionB=(xB,yB)}:bs) bsDone 
  | (inCollisionP i xP spW xB bW yP spH yB bH) == True = ((updatePlayer 'h' sp), (bsDone ++ bs))
  | otherwise = collisionCheckEnemyBullets i sp bs (bsDone ++ [b])

collisionCheckEnemy :: Int -> Player -> [Enemy] -> [Enemy] -> (Player, [Enemy])
collisionCheckEnemy i sp [] x = (sp, x)
collisionCheckEnemy i sp@Player{health=h,position=(xP,yP)} (e@Enemy{positionE=(xE,yE),sizeE=(eH,eW)}:es) esDone 
  | (inCollisionP i xP spW xE eW yP spH yE eH) == True = ((updatePlayer 'h' sp), (esDone ++ es))
  | otherwise = collisionCheckEnemy i sp es (esDone ++ [e])

collisionBulEnem1v1 :: GameState -> GameState
collisionBulEnem1v1 GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime=tt}
  = GameState (Show1v1 c1 sp1 c2 sp2 (fst2 check1) (fst2 check2) (snd2 check1) (snd2 check2)) tt
    where check1 = collisionBullets2 bs1 es1 [] 0 (-1)
          check2 = collisionBullets2 bs2 es2 [] 0 (-2)

--Bekijkt de collisions tussen de speler's bullets en de enemies. Hij past de bullets, enemies en score aan
--o.b.v. of er enemies gedood zijn.
collBulEnem :: GameState -> GameState
collBulEnem GameState{infoToShow = ShowFinal c sp bs es ebs sc, elapsedTime = tt} =
  GameState (ShowFinal c sp (fst2 check) (snd2 check) ebs (sc + thrd check)) tt
    where check = collisionBullets2 bs es [] 0 (-1)
collBulEnem GameState{infoToShow = ShowLevel c sp bs es ebs sc t l, elapsedTime = tt} =
  GameState (ShowLevel c sp (fst2 check) (snd2 check) ebs (sc + thrd check) t l) tt
    where check = collisionBullets2 bs es [] 0 (-1)
collBulEnem gs = gs

collisionBullets2 :: [Bullet] -> [Enemy] -> [Bullet] -> Float -> Int -> ([Bullet], [Enemy], Float)
collisionBullets2 [] x bsDone sc i = (bsDone, x, sc)
collisionBullets2 x [] bsDone sc i = ((bsDone ++ x), [], sc)
collisionBullets2 (b:bs) es bsDone sc i
  | fst2 check == True = collisionBullets2 bs (snd2 check) (bsDone ++ [b]) sc i
  | otherwise = collisionBullets2 bs (snd2 check) bsDone (thrd check) i
     where check = collisionFree2 b es [] i

collisionFree2 :: Bullet -> [Enemy] -> [Enemy] -> Int -> (Bool, [Enemy], Float)
collisionFree2 _ [] esDone i = (True, esDone, 0)
collisionFree2 b@Bullet{positionB = (xB, yB)} (e@Enemy{positionE = (xE, yE), sizeE = (eH, eW)}:es) esDone i
  | (inCollisionP i xB bW xE eW yB bH yE eH) == True = (False, esDone ++ (fst check) ++ es, snd check)
  | otherwise = collisionFree2 b es (esDone ++ [e]) i
    where check = enemyHit e

--Bekijkt of een Enemy en de speler hebben gebotst(collided).
enemyTouchesPlayer :: [Enemy] -> Player -> [Enemy] -> (Bool, [Enemy])
enemyTouchesPlayer [] _ enemiesDone = (False, enemiesDone)
enemyTouchesPlayer (e@Enemy{positionE = (xE,yE), sizeE=(eH,eW)}:enemies) speler@Player{health=h,position=(xP,yP)} enemiesDone 
  -- | inCollisionX xP spW xE eW && abs (yP-yE) < (eH/2 + ((xP + abs xE) * (spH/2/spW))) =
  | (inCollisionP 1 xP spW xE eW yP spH yE eH) == True = 
    (True, enemiesDone ++ enemies)
  | otherwise = enemyTouchesPlayer enemies speler (enemiesDone ++ [e])
