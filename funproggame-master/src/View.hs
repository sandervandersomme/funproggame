-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Bullet
import Enemy
import Constanten
import System.Environment


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing -> color red (text "start")
  ShowGameState Player { speed = s, health = h, position = (x,y)} bullets -> 
    pictures [translate x y (color red (ThickCircle 20 40))]
  ShowPause -> color red (text "pauze")
  ShowTest Player{speed = s, health = h, position = (x,y)} bullets enemies -> 
    pictures [translate x y (color red (ThickCircle 20 40)), bulletPics bullets, enemyPics enemies]
  ShowTriangle Player{speed = s, position = (x,y)} -> 
    color azure $ Polygon [(0,0), (-40, -15), (-32, 0), (-40, 15), (0,0)]

  ShowFinal _ sp@Player{speed=s, position=(x,y), health=hp} bullets enemies enemBullets score ->
    pictures [translate x y (spelerPic sp), bulletPics bullets, enemyPics enemies, scoreText score, healthText hp]
  
  ShowMenu -> pictures [textToPicture 0.5 0.5 (-400) 300 red "1 vs. 1 (1)", 
    textToPicture 0.5 0.5 (-400) 150 red "Normal mode (u)", 
    textToPicture 0.5 0.5 (-400) 300 red "Level select (l)",
    textToPicture 0.5 0.5 (-400) 0 red "Battle against time (t)",
    textToPicture 0.5 0.5 (-400) (-150) red "Highscores"]

  --ShowANumber n -> color blue (text (show n))
  --ShowAChar   c -> color red (text [c])

  ShowHighScores -> pictures [textToPicture 0.5 0.5 (-1000) 500 red "Top 5 Highest scores:"] 
  --  highscoresText getHighscores]

  ShowDead speler@Player{speed=s, position=(x,y)} enemies score -> pictures [translate x y (spelerPic speler),  
    enemyPics enemies,
    scoreText score,
    textToPicture 0.5 0.5 (-800) (-150) red "Game Over",
    textToPicture 0.5 0.5 (-800) (-350) red "Press H to see Highscores"]

--Als deze wordt aangepast moet collision ook aangepast worden
--spelerPic :: Picture
--spelerPic = color azure $ Polygon ([(0,0), (-playerWidth, -playerHeight2), (-64, 0), (-playerWidth, playerHeight2), (0,0)])
spelerPic :: Player -> Picture
spelerPic Player{immuCou = ic} 
  | ic == 0   = color azure $ Polygon [(0,0), (-playerWidth, -playerHeight2), (-64, 0), (-playerWidth, playerHeight2), (0,0)]
  | otherwise = color (greyN 0.5) $ Polygon [(0,0), (-playerWidth, -playerHeight2), (-64, 0), (-playerWidth, playerHeight2), (0,0)]

scoreText :: Float -> Picture
-- scoreText x = scale 0.5 0.5 $ translate 0 (-100) $ color red (text (show x))
scoreText x = textToPicture 0.5 0.5 0 (0) red (show x)

healthText :: Int -> Picture
-- scoreText x = scale 0.5 0.5 $ translate 0 (-100) $ color red (text (show x))
healthText x = textToPicture 0.5 0.5 0 (-200) green (show x)

textToPicture :: Float -> Float -> Float -> Float -> Color -> String -> Picture
textToPicture s1 s2 t1 t2 colour tekst = scale s1 s2 $ translate t1 t2 $ color colour $ text tekst

bulletPics :: [Bullet] -> Picture
bulletPics [] = pictures []
bulletPics bullets = pictures [bulletToPic b | b <- bullets]

bulletToPic :: Bullet -> Picture
bulletToPic Bullet{speedB = s, positionB = (x,y), colourB = c} = translate x y (color c bulletPic)

bulletPic :: Picture
bulletPic = Polygon [(0,0), (0,-bulletHeight/2), (-bulletWidth, -bulletHeight/2),(-bulletWidth,bulletHeight/2),(0,bulletHeight/2),(0,0)]

enemyPics :: [Enemy] -> Picture
enemyPics [] = pictures []
enemyPics bullets = pictures [enemyToPic b | b <- bullets]

enemyToPic :: Enemy -> Picture
enemyToPic Enemy{speedE=s, positionE=(x,y), healthE=h} = translate x y (color white 
  $ Polygon [(0,0), (0, enemy1Height2), (enemy1Width,enemy1Height2), (enemy1Width,-enemy1Height2), (0,-enemy1Height2), (0,0)])

--test1 :: IO ()
--test1 = do
--  h <- openFile "" ReadMode
--  hGetContents
--  [f,g] <- getArgs
--  s     <- readFile f
--  writeFile g s

readInt :: String -> Int
readInt = read

readHighscores :: IO()
readHighscores = do  
  contents <- readFile "highscores.txt"
  print . map readInt . lines $ contents

createHighScores :: Int -> IO()
createHighScores = writeFile "highscores.txt"