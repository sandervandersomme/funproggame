module Updates where

import Constanten
import Player
import Bullet
import Enemy
import Model


updateGameDead :: String -> GameState -> GameState
updateGameDead s gs@GameState{infoToShow=ShowDead speler enemies sc _} 
  = GameState (ShowDead speler enemies sc s) 0
updateGameDead _ gs = gs

--Update de locatie van de bullets. Iedere bullet gaat met s (speed) naar rechts
updateBullets :: [Bullet] -> [Bullet]
updateBullets [] = []
updateBullets (Bullet{speedB=s,positionB=(x,y),colourB=c}:bs) 
  | (x - bW) < 600 = (Bullet s (x+s,y) c) : updateBullets bs
  | otherwise      = updateBullets bs

updateBullets2 :: [Bullet] -> [Bullet]
updateBullets2 [] = []
updateBullets2 (Bullet{speedB=s,positionB=(x,y),colourB=c}:bs) 
  | (x + bW) > (-600) = (Bullet s (x-s,y) c) : updateBullets2 bs
  | otherwise         = updateBullets2 bs

--Update de locatie van de Enemies. Iedere enemy gaat met s (speed) naar links
updateEnemies :: [Enemy] -> [Enemy]
updateEnemies [] = []
updateEnemies (Enemy{speedE=s,positionE=(x,y),healthE=h,sizeE=(eH,eW),typeE=t,colour=co,bullC=bc}:es) 
  | (x + eW) > (-600) = (Enemy s (x-s,y) h (eH,eW) t co bc) : updateEnemies es
  | otherwise         = updateEnemies es

updateEnemies2 :: [Enemy] -> [Enemy]
updateEnemies2 [] = []
updateEnemies2 (Enemy{speedE=s,positionE=(x,y),healthE=h,sizeE=(eH,eW),typeE=t,colour=co,bullC=bc}:es) 
  | (x - eW) < 600 = (Enemy s (x+s,y) h (eH,eW) t co bc) : updateEnemies2 es
  | otherwise      = updateEnemies2 es

--Update de speler als w/s ingedrukt is, maar ook als de speler geraakt is.
updatePlayer :: Char -> Player -> Player
updatePlayer 'w' Player{speed=s, health=h, position=(x,y), bullCou=bc, immuCou=ic} 
  = Player s h (x, min (400-spH/2) (y+s)) (max 0 (bc-1)) (max 0 (ic-1))
updatePlayer 's' Player{speed=s, health=h, position=(x,y), bullCou=bc, immuCou=ic}
  = Player s h (x, max (-400+spH/2) (y-s)) (max 0 (bc-1)) (max 0 (ic-1))
updatePlayer 'h' Player{speed=s, health=h, position=p, bullCou=bc, immuCou=ic}
  = Player s (h-1) p bc immunityCounter
updatePlayer _ Player{speed=s, health=h, position=(x,y), bullCou=bc, immuCou=ic}
  = Player s h (x, y) (max 0 (bc-1)) (max 0 (ic-1))