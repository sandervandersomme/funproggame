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
import Level
import System.IO
import Control.Monad
import Updates
import Collisions
import HulpFuncties

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES && gstateIsShowdead gstate
  = 
  do
    contents <- readFile "highscore.txt"
    laatsteScore <- getScore gstate
    huidigeScore <- getScore gstate
    let scoreText = words $ contents
    let scores = updateHighScores (round huidigeScore) $ scoreString scoreText
    let tekst = scoresToString scores
    return $ updateGameDead tekst gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES && gstateIsShowhighscores gstate
  = 
  do
    let tekst = getHighScoreText gstate
    -- if tekst /= "" (test <- writeFile "highscore.txt" tekst)
    _ <- schrijfTekst tekst
    --contents <- readFile "highscore.txt"
    --let woorden = words $ contents
    --let woorden = ["werkt nog niet"]
    return $ GameState (ShowHighScores woorden "_") 0
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES && gstateIsShowLevel gstate
  =
  do
    i <- aantalEnemies gstate 
    r1 <- randomIO :: IO Float
    r2 <- randomIO :: IO Float
    r3 <- randomIO :: IO Float
    r4 <- randomIO :: IO Float
    r5 <- randomIO :: IO Float
    r6 <- randomIO :: IO Float
    r7 <- randomIO :: IO Float
    r8 <- randomIO :: IO Float
    r9 <- randomIO :: IO Float
    r10 <- randomIO :: IO Float
    r11 <- randomIO :: IO Float
    r12 <- randomIO :: IO Float
    r13 <- randomIO :: IO Float
    return $ updateGSLevel (take i [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13]) gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES
  =
  do
    ranNum <- randomIO :: IO Float
    return $ updateGameState ranNum gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

schrijfTekst :: String-> GameState -> IO ()
schrijfTekst s gs | s == ""
  = return ()
  | otherwise = 
    do test <- writeFile "highscore.txt" s
       return ()


updateGameState :: Float -> GameState -> GameState
updateGameState ran gs@GameState{infoToShow = ShowFinal _ _ _ _ _ _} 
  = collBulEnem $ collisionChecker $ newGsT ran gs
updateGameState ran gs@GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime=tt}
  -- | ran <= (eSpPr/3) = collisionBulEnem1v1 $ collisionChecker $ addEnemy (ran/(eSpPr/3)) gs
  | ran <= (eSpPr/3) =  collisionChecker2 $ collisionBulEnem1v1 $ collisionChecker $ addEnemy (ran/(eSpPr/3)) gs
  -- | otherwise = collisionBulEnem1v1 $ collisionChecker $ updateGameState1v1 gs
  | otherwise = collisionChecker2 $ collisionBulEnem1v1 $ collisionChecker $ updateGameState1v1 gs
updateGameState ran gs@GameState{infoToShow = ShowLevel c1 sp bs es ebs sc t l}
  = collBulEnem $ collisionChecker $ GameState (ShowLevel c1 (updatePlayer c1 sp) 
    (updateBullets bs) (updateEnemies es) ebs sc (t+1) l) 0
updateGameState ran gs = collBulEnem $ collisionChecker $ newGsT ran gs
 -- = collBulEnem $ collisionChecker $ 


updateGameState1v1 :: GameState -> GameState
updateGameState1v1 gs@GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime=tt}
  = GameState (Show1v1 c1 (updatePlayer c1 sp1) c2 (updatePlayer c2 sp2) (updateBullets bs1) 
    (updateBullets2 bs2) (updateEnemies es1) (updateEnemies2 es2)) tt
updateGameState1v1 gs = gs

 --(t + (nOSECSBETWEENCYCLES*2))
updateGSLevel :: [Float] -> GameState -> GameState
updateGSLevel rands gs@GameState{infoToShow=ShowLevel c1 sp bs es ebs sc t l@Level{idL=idL1,paren=(p:ps)}}   
  | t >= 100
    = updateGameState 0 $ GameState (ShowLevel c1 sp bs (es ++ 
      (spawnEnemies (fromIntegral (length rands)) rands p)) ebs sc 0 (Level idL1 ps)) 0
  | otherwise = updateGameState 0 gs--geen nieuwe enemies spawnen
updateGSLevel rands gs@GameState{infoToShow=ShowLevel c1 sp bs es ebs sc t l@Level{idL=idL1,paren=[]}}   
  = updateGameState 0 gs--geen nieuwe enemies spawnen
updateGSLevel (r:rs) gs = updateGameState r gs
updateGSLevel _ gs = updateGameState 0 gs

extractParen :: Level -> [Paar]
extractParen l@Level{idL=idL1,paren=p} = p

spawnEnemies :: Float -> [Float] -> Paar -> [Enemy]
spawnEnemies _ _ (l, 0, e@Enemy{speedE=s,healthE=h,sizeE=(eH,eW),typeE=t,colour=co}) = []
spawnEnemies x (r:rs) (l, 1, e@Enemy{speedE=s,healthE=h,sizeE=(eH,eW),typeE=t,colour=co}) = 
  [(Enemy s ((600 + (x - 1)*20*s/x), (randomY 1 r eH)) h (eH,eW) t co 0)]
spawnEnemies x (r:rs) (l, i, e@Enemy{speedE=s,healthE=h,sizeE=(eH,eW),typeE=t,colour=co}) = 
  (Enemy s ((600 + (x - (fromIntegral i))*20*s/x), (randomY 1 r eH)) h (eH,eW) t co 0) 
    : spawnEnemies x rs (l, (i-1), e)
spawnEnemies _ _ _ = []

--ran / eSpPr
addEnemy :: Float -> GameState -> GameState
addEnemy ran gs@GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime=tt}
  | ran < e2SpPr/2 = GameState (Show1v1 c1 (updatePlayer c1 sp1) c2 (updatePlayer c2 sp2) (updateBullets bs1) 
    (updateBullets2 bs2) ((updateEnemies es1) ++ [(createEnemy ran 2 1 (e2SpPr/2))]) (updateEnemies2 es2)) tt
  | ran < e2SpPr   = GameState (Show1v1 c1 (updatePlayer c1 sp1) c2 (updatePlayer c2 sp2) (updateBullets bs1) 
    (updateBullets2 bs2) (updateEnemies es1) 
    ((updateEnemies2 es2) ++ [(createEnemy (ran-(e2SpPr/2)) 2 (-1) (e2SpPr/2))]) ) tt
  | ran < ((e1SpPr/2) + e2SpPr) = GameState (Show1v1 c1 (updatePlayer c1 sp1) c2 (updatePlayer c2 sp2) 
    (updateBullets bs1) (updateBullets2 bs2) 
    ((updateEnemies es1) ++ [(createEnemy (ran-e2SpPr) 1 1 (e1SpPr/2))]) (updateEnemies2 es2)) tt
  | ran < (e1SpPr + e2SpPr) = GameState (Show1v1 c1 (updatePlayer c1 sp1) c2 (updatePlayer c2 sp2) (updateBullets bs1) 
    (updateBullets2 bs2) (updateEnemies es1) 
    ((updateEnemies2 es2) ++ [(createEnemy (ran - (e1SpPr/2 + e2SpPr)) 1 (-1) (e1SpPr/2))]) ) tt
  | otherwise = gs

--Random waarde -> type -> side
createEnemy :: Float -> TypeEnemy -> Float -> Float -> Enemy
createEnemy ran 1 side kans = Enemy e1S ((600*side), (randomY kans ran e1H)) 
  2 (e1H,e1W) 1 (getColour (side-2)) 0
createEnemy ran 2 side kans = Enemy e2S ((600*side), (randomY kans ran e2H)) 
  5 (e2H,e2W) 2 (getColour (side-2)) 0

--spawn probability -> random float
randomY :: Float -> Float -> Float -> Float
randomY spPr ran enemH = (((fromIntegral screenHeight)/2 - enemH/2) / (spPr/2)) * (ran - (spPr/2))

--Update iedere 'nOSECSBETWEENCYCLES' seconde
--Zorgt ervoor dat de bullets bewegen, dat de enemies bewegen.
--En bij ShowFinal zorgt het ervoor dat ook de speler blijft bewegen als w of s ingedrukt is.
newGsT :: Float -> GameState -> GameState
newGsT randN GameState{infoToShow = ShowFinal c sp bs es ebs sc}
  | randN <= eSpPr = GameState (ShowFinal c (updatePlayer c sp) (updateBullets bs) 
    ((updateEnemies es) ++ [(Enemy 3 (eSpX, (berekenRandomY randN)) 1 (e1H,e1W) 1 (getColour 1) 0)]) ebs sc) 0
  | otherwise      = GameState (ShowFinal c (updatePlayer c sp) (updateBullets bs) (updateEnemies es) ebs sc) 0
newGsT randN GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2}
  = GameState (Show1v1 c1 (updatePlayer c1 sp1) c2 (updatePlayer c2 sp2) (updateBullets bs1) 
    (updateBullets2 bs2) (updateEnemies es1) (updateEnemies es2)) 0
newGsT _ gs = gs

--random getal zorgt voor spreiding over y as. Dus 10% kans op nieuwe enemy betekent:
--(hoogte display/2 - hoogte enemy/2) / 0.05 = x
--Momenteel is dat 800/2 - 20/2 = 390 / 0.05 = 7800
berekenRandomY :: Float -> Float
berekenRandomY ran = (((fromIntegral screenHeight)/2 - e1H/2) / (eSpPr/2)) * (ran - (eSpPr/2))



-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) pr _ _) gstate
  -- If the user presses a character key, show that one
   | c == 'p' && pr == Down = updateInfoToShow 'p' gstate 
   | c == 'w' && pr == Up   = unpressKey c gstate 
   | c == 'w' && pr == Down = playerMoved 1 c gstate 
   | c == 's' && pr == Up   = unpressKey c gstate 
   | c == 's' && pr == Down = playerMoved 1 c gstate 
   | c == 'z' && pr == Down = bulletFired gstate
   | c == 'l' && pr == Down = updateInfoToShow 'l' gstate
   | c == 'u' && pr == Down = updateInfoToShow 'u' gstate 
   | c == '1' && pr == Down = updateInfoToShow '1' gstate
   | c == '0' && pr == Down = updateInfoToShow '0' gstate
   | c == 'h' && pr == Down = updateInfoToShow 'h' gstate
   | otherwise = gstate
inputKey (EventKey (SpecialKey s) pr _ _) gstate
   | s == KeyUp && is1v1 gstate && pr == Up = unpressKey '^' gstate
   | s == KeyUp && is1v1 gstate && pr == Down = press1v1 'w' 2 gstate
   | s == KeyDown && is1v1 gstate && pr == Up = unpressKey 'v' gstate
   | s == KeyDown && is1v1 gstate && pr == Down = press1v1 's' 2 gstate
   | s == KeyLeft && is1v1 gstate && pr == Down = bullet1v1 2 gstate
inputKey _ gstate = gstate -- Otherwise keep the same

is1v1 :: GameState -> Bool
is1v1 GameState{infoToShow=Show1v1 _ _ _ _ _ _ _ _} = True
is1v1 _ = False

updateInfoToShow :: Char -> GameState -> GameState
updateInfoToShow 'l' GameState{infoToShow=ShowMenu c i} 
  | c == 'l'  = GameState (ShowMenu '_' (3-i)) 0
  | otherwise = GameState (ShowMenu 'l' (3-i)) 0
updateInfoToShow '1' GameState{infoToShow=ShowMenu c i} 
  | c == 'l'  = GameState (ShowLevel '_' (Player 8 3 (spX, spY) 0 0) [] [] [] 0 0 level1 ) 0 
  | otherwise = GameState (Show1v1 '_' player1 '_' player2 [] [] [] []) 0
updateInfoToShow '0' GameState{infoToShow=ShowMenu c i} 
  = GameState (ShowLevel '_' (Player 5 3 (spX, spY) 0 0) [] [] [] 0 0 level0 ) 0 
updateInfoToShow 'h' gs@GameState{infoToShow=ShowDead _ _ sc s} = GameState (ShowHighScores ["",""] s) 0
updateInfoToShow 'h' gs = GameState (ShowHighScores ["a", "b"] "_") 0
updateInfoToShow 'u' GameState{infoToShow=ShowMenu _ _} 
  = GameState (ShowFinal '_' (Player 5 3 (spX, spY) 0 0) [] [] [] 0) 0
updateInfoToShow 'p' gs@GameState{infoToShow=ShowPause x y} = GameState x y
updateInfoToShow 'p' gs@GameState{infoToShow=i, elapsedTime=tt} = GameState (ShowPause i tt) 0
updateInfoToShow _ gs = gs

unpressKey :: Char -> GameState -> GameState 
unpressKey '^' GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt}
  = GameState (Show1v1 c1 sp1 (verwijderChar 'w' c2) sp2 bs1 bs2 es1 es2) tt
unpressKey 'v' GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt}
  = GameState (Show1v1 c1 sp1 (verwijderChar 's' c2) sp2 bs1 bs2 es1 es2) tt
unpressKey c GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt}
  = GameState (Show1v1 (verwijderChar c c1) sp1 c2 sp2 bs1 bs2 es1 es2) tt
unpressKey c GameState{infoToShow = ShowFinal c1 speler bs es ebs sc, elapsedTime = tt}
  = GameState (ShowFinal (verwijderChar c c1) speler bs es ebs sc) tt
unpressKey c GameState{infoToShow = ShowLevel c1 speler bs es ebs sc t l, elapsedTime = tt}
  = GameState (ShowLevel (verwijderChar c c1) speler bs es ebs sc t l) tt
unpressKey c gs = gs

playerMoved :: Int -> Char -> GameState -> GameState
playerMoved sp c gs@GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2} 
  = press1v1 c 1 gs
playerMoved _ cPr GameState{infoToShow = ShowFinal c sp@Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bs es ebs sc, elapsedTime=tt} 
  = GameState (ShowFinal (updateChar cPr c) sp bs es ebs sc) tt 
playerMoved _ cPr GameState{infoToShow = ShowLevel c sp@Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bs es ebs sc t l, elapsedTime=tt} 
  = GameState (ShowLevel (updateChar cPr c) sp bs es ebs sc t l) tt 
playerMoved _ _ gs = gs

bulletFired :: GameState -> GameState
bulletFired gs@GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2} = bullet1v1 1 gs
bulletFired gs@GameState{infoToShow = ShowFinal c Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bs es ebs sc, elapsedTime = tt}
    | bc < 1   = GameState (ShowFinal c (Player s h (x, y) bulletCounter ic) 
      (bs ++ [(Bullet bullS (x + 10, y) blue)]) es ebs sc) tt
    | otherwise = gs
bulletFired gs@GameState{infoToShow = ShowLevel c sp@Player{speed=s, health=h, position=(x, y), 
  bullCou=bc, immuCou=ic} bs es ebs sc t l, elapsedTime=tt} 
    | bc < 1 = GameState (ShowLevel c (Player s h (x, y) bulletCounter ic) 
      (bs ++ [(Bullet bullS (x + 10, y) blue)]) es ebs sc t l) tt
    | otherwise = gs
bulletFired gs = gs

updateChar :: Char -> Char -> Char
updateChar 'w' 's' = '+'
updateChar 's' 'w' = '+'
--updateChar c   '_' = c
updateChar c b | c == b = '_' 
               | otherwise = c

verwijderChar :: Char -> Char -> Char
verwijderChar 'w' '+' = 's'
verwijderChar 's' '+' = 'w'
verwijderChar 'w' 'w' = '_'
verwijderChar 's' 's' = '_'
verwijderChar  _   _  = '_' 

press1v1 :: Char -> Int -> GameState -> GameState
press1v1 c 1 GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt} 
  = GameState (Show1v1 (updateChar c c1) sp1 c2 sp2 bs1 bs2 es1 es2) tt
press1v1 c 2 GameState{infoToShow = Show1v1 c1 sp1 c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt} 
  = GameState (Show1v1 c1 sp1 (updateChar c c2) sp2 bs1 bs2 es1 es2) tt

bullet1v1 :: Int -> GameState -> GameState
bullet1v1 1 gs@GameState{infoToShow = Show1v1 c1 sp1@Player{speed=s,health=h,position=(x, y), 
  bullCou=bc,immuCou=ic} c2 sp2 bs1 bs2 es1 es2, elapsedTime = tt} 
    | bc < 1 = GameState (Show1v1 c1 (Player s h (x,y) bulletCounter1v1 ic) c2 sp2 
      (bs1 ++ [(Bullet bullS (x+10,y) blue)]) bs2 es1 es2) tt
    | otherwise = gs
bullet1v1 2 gs@GameState{infoToShow = Show1v1 c1 sp1 c2 sp2@Player{speed=s,health=h,position=(x, y), 
  bullCou=bc,immuCou=ic} bs1 bs2 es1 es2, elapsedTime = tt} 
    | bc < 1 = GameState (Show1v1 c1 sp1 c2 (Player s h (x,y) bulletCounter1v1 ic) 
      bs1 (bs2 ++ [(Bullet bullS (x-10,y) red)]) es1 es2) tt
    | otherwise = gs
