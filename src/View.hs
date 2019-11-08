-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Bullet
import Enemy
import Constanten

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing -> color red (text "start")
  ShowGameState Player { speed = s, health = h, position = (x,y)} bs -> 
    pictures [translate x y (color red (ThickCircle 20 40))]
  ShowPause -> color red (text "pauze")
  ShowTest Player{speed = s, health = h, position = (x,y)} bs es -> 
    pictures [translate x y (color red (ThickCircle 20 40)), bulletPics bs, enemyPics es]
  ShowTriangle Player{speed = s, health = h, position = (x,y)} -> 
    --color azure $ Polygon ([(-8, 0), (-15, 10), (15, 0), (-15,-10), (-8, 0)])
    color azure $ Polygon ([(0,0), (-40, -15), (-32, 0), (-40, 15), (0,0)])

  ShowFinal _ Player{speed=s, health=h, position=(x,y)} bs es ebs score ->
    pictures [translate x y (spelerPic), bulletPics bs, enemyPics es, scoreText score, ]
    
  --ShowANumber n -> color blue (text (show n))
  --ShowAChar   c -> color red (text [c])

--Als deze wordt aangepast moet collision ook aangepast worden
spelerPic :: Picture
spelerPic = color azure $ Polygon ([(0,0), (-spW, -spH2), (-64, 0), (-spW, spH2), (0,0)])

scoreText :: Float -> Picture
scoreText x = scale 0.5 0.5 $ translate 0 (-100) $ color red (text (show x))

bulletPics :: [Bullet] -> Picture
bulletPics [] = pictures []
bulletPics bs = pictures [bulletToPic b | b <- bs]

bulletToPic :: Bullet -> Picture
bulletToPic Bullet{speedB = s, positionB = (x,y), colourB = c} = translate x y (color c bulletPic)

bulletPic :: Picture
bulletPic = Polygon ([(0,0), (0,-bH/2), (-bW, -bH/2),(-bW,bH/2),(0,bH/2),(0,0)])

enemyPics :: [Enemy] -> Picture
enemyPics [] = pictures []
enemyPics bs = pictures [enemyToPic b | b <- bs]

enemyToPic :: Enemy -> Picture
enemyToPic Enemy{speedE=s, positionE=(x,y), healthE=h} = translate x y (color white 
  $ Polygon ([(0,0), (0, e1H2), (e1W,e1H2), (e1W,-e1H2), (0,-e1H2), (0,0)]))