-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Bullet
import Enemy
import Constanten
import Level

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing -> color red (text "start")
  ShowPause _ _ -> color red (text "pauze")
  ShowFinal _ sp@Player{speed=s, position=(x,y)} bs es ebs score ->
    pictures [(spelerToPic azure sp), bulletPics bs, enemyPics es, scoreText score ]
  ShowMenu c i -> menuPic i
  Show1v1 _ sp1 _ sp2@Player{position=(x,y)} bs bs2 es es2 -> pictures [bulletPics bs, bulletPics bs2, enemyPics es, enemyPics es2,
    (spelerToPic azure sp1),
    (spelerToPic orange sp2)
    --,(bulletToPic (Bullet 1 (x,0) red)), (bulletToPic (Bullet 1 (x,20) blue)),
    --(enemyToPic (Enemy 1 (x, 50) 1 (20,20) 1 azure)), (enemyToPic (Enemy 1 (x, 100) 1 (20,20) 1 orange)),
    --(enemyToPic (Enemy 1 (x, -100) 1 (80,80) 2 azure)), (enemyToPic (Enemy 1 (x, -300) 1 (80,80) 2 orange))
     ]
  ShowLevel _ sp bs es ebs score t lev ->
    pictures [(spelerToPic azure sp), bulletPics bs, enemyPics es, 
      (textToPicture 0.5 0.5 0 (300) red (show t)), levelToPic lev, scoreText score ]
  ShowHighScores hs s -> pictures [(highscoresToPic hs s)]
  ShowDead speler@Player{speed=s, position=(x,y)} enemies score _ -> pictures [
    (spelerToPic azure speler),
    --translate x y (spelerPic speler),  
    enemyPics enemies,
    scoreText score,
    textToPicture 0.5 0.5 (-800) (100) red "Game Over",
    textToPicture 0.5 0.5 (-1000) (-100) red "Press H to sumbit your score and see the Highscores "]

spelerToPic :: Color -> Player -> Picture
spelerToPic c Player{position=(x,y), immuCou=ic} | x < 0 = translate x y $ spelerPic ic c
                                                 | otherwise = translate x y $ rotate 180 $ spelerPic ic c

spelerPic :: Int -> Color -> Picture
spelerPic 0 c = color c spelerPolygon
spelerPic x _ | functie x == 0 = color (greyN 0.7) spelerPolygon
              | functie x == 1 = color (greyN 0.3) spelerPolygon
                where functie x = mod (round ((fromIntegral x) / 5)) 2

spelerPolygon :: Picture
spelerPolygon = Polygon ([(0,0), (-spW, -spH/2), (-64, 0), (-spW, spH/2), (0,0)])

scoreText :: Float -> Picture
scoreText x = textToPicture 0.5 0.5 0 (-100) red (show x)

healthText :: Int -> Picture
healthText x = textToPicture 0.5 0.5 0 (-200) green (show x)

highscoresToPic :: [String] -> String -> Picture
highscoresToPic (h1:h2:h3:h4:h5:hs) s | s /= "_" = highscoresToPic (lines s) "_"
  | otherwise =  pictures [
  (textToPicture 0.5 0.5 (-200) (500) red "Highscores:"),
  (textToPicture 0.5 0.5 (-200) (300) red ("1. " ++ h1)), 
  (textToPicture 0.5 0.5 (-200) (100) red ("2. " ++ h2)),
  (textToPicture 0.5 0.5 (-200) (-100) red ("3. " ++ h3)),
  (textToPicture 0.5 0.5 (-200) (-300) red ("4. " ++ h4)),
  (textToPicture 0.5 0.5 (-200) (-500) red ("5. " ++ h5))]
highscoresToPic _ _ = (textToPicture 0.5 0.5 0 (-100) red "faal")

textToPicture :: Float -> Float -> Float -> Float -> Color -> String -> Picture
textToPicture s1 s2 t1 t2 colour tekst = scale s1 s2 $ translate t1 t2 $ color colour $ text tekst

bulletPics :: [Bullet] -> Picture
bulletPics [] = pictures []
bulletPics bs = pictures [bulletToPic b | b <- bs]

bulletToPic :: Bullet -> Picture
bulletToPic Bullet{speedB = s, positionB = (x,y), colourB = c} 
  | c == blue = translate x y $ color blue bulletPic
  | c == red  = translate x y $ rotate 180 $ color red bulletPic

bulletPic :: Picture
bulletPic = Polygon ([(0,0), (0,-bH/2), (-bW, -bH/2),(-bW,bH/2),(0,bH/2),(0,0)])

enemyPics :: [Enemy] -> Picture
enemyPics [] = pictures []
enemyPics es = pictures [enemyToPic e | e <- es]

enemyToPic :: Enemy -> Picture
enemyToPic Enemy{speedE=s, positionE=(x,y), healthE=h, typeE=t, colour=co} 
  | t == 1 && co == azure = translate x y (color co
    $ Polygon ([(0,0), (0, e1H/2), (-e1W,e1H/2), (-e1W,-e1H/2), (0,-e1H/2), (0,0)]))
  | t == 1 = translate x y (color co
    $ Polygon ([(0,0), (0, e1H/2), (e1W,e1H/2), (e1W,-e1H/2), (0,-e1H/2), (0,0)]))
  | t == 2 && co == azure = translate x y (color co
    $ Polygon ([(0,0), (0, e2H/2), (-e2W,e2H/2), (-e2W,-e2H/2), (0,-e2H/2), (0,0)]))
  | t == 2 = translate x y (color co
    $ Polygon ([(0,0), (0, e2H/2), (e2W,e2H/2), (e2W,-e2H/2), (0,-e2H/2), (0,0)]))

levelToPic :: Level -> Picture
levelToPic Level{idL=i} = textToPicture 0.5 0.5 100 400 red (show i)

menuPic :: Int -> Picture
menuPic 1 = standaardMenu
menuPic 2 = pictures [(textToPicture 0.5 0.5 (-1000) 400 red "Level Select, press 1, 2 or 3"), 
  (textToPicture 0.5 0.5 (-500) 200 red "press l to go back")]

standaardMenu :: Picture
standaardMenu = pictures [(textToPicture 0.5 0.5 (-400) 400 red "1 vs. 1 (1)"), 
    (textToPicture 0.5 0.5 (-400) 200 red "Normal mode (u)"), 
    (textToPicture 0.5 0.5 (-400) 0 red "Level select (l)"),
    (textToPicture 0.5 0.5 (-400) (-200) red "Battle against time (t)"),
    (textToPicture 0.5 0.5 (-400) (-400) red "Highscores (h)") ]

