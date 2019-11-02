-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing -> color red (text "start")
  ShowGameState Player { speed = s, health = h, position = (x,y)}  -> translate x y (color red (ThickCircle 20 40))
  ShowPause -> color red (text "pauze")
  --ShowANumber n -> color blue (text (show n))
  --ShowAChar   c -> color red (text [c])