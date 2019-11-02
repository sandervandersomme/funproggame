-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player
import Bullet

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing -> color red (text "start")
  ShowGameState Player { speed = s, health = h, position = (x,y)} bs -> 
    pictures [translate x y (color red (ThickCircle 20 40))]
  ShowPause -> color red (text "pauze")
  ShowBullets bs@[Bullet{speedB = s, positionB = (x,y), colourB = c}] -> translate x y (color c (ThickCircle 5 10))
  --ShowANumber n -> color blue (text (show n))
  --ShowAChar   c -> color red (text [c])