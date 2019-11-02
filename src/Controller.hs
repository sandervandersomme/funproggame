-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Player
import Bullet

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES
  =
  do
    --let speler = Player 0 3 (-350, 0)
    --return $ GameState (ShowGameState speler) 0 
    return $ newGstate gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

newGstate :: GameState -> GameState
newGstate GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = _}
   = GameState (ShowGameState (Player s h (x, y)) bs) 0
newGstate GameState{infoToShow = ShowBullets [Bullet{speedB=s,positionB=(x,y),colourB=c}]}
   = GameState (ShowBullets [b]) 0
     where b = (Bullet s (x+s, y) c)
newGstate gs = gs

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
   | c == 'w' && pr == Down = vgsW gstate
   | c == 's' && pr == Down = vgsS gstate
   | c == 'b' = gstate { infoToShow = ShowBullets [(Bullet 10 (-350, 0) red)]}
   | otherwise = gstate
--   = gstate { infoToShow = ShowPause }
inputKey _ gstate = gstate -- Otherwise keep the same

vgsW :: GameState -> GameState
-- min 400 y+10 zodat hij niet buiten het veld gaat
vgsW GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = tt}
   = GameState (ShowGameState (Player s h (-400, (min 400 y+10))) bs) tt
vgsW gs = gs

vgsS :: GameState -> GameState
vgsS GameState{infoToShow = ShowGameState Player{speed=s, health=h, position=(x, y)} bs, elapsedTime = tt}
   = GameState (ShowGameState (Player s h (-400, (max (-400) y-10))) bs) tt
vgsS gs = gs