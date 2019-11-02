-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Player

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nOSECSBETWEENCYCLES
  =
  do
    let speler = Player 0 3 (-400, 0)
    return $ GameState (ShowGameState speler) 0 
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  -- If the user presses a character key, show that one
  | c == 'p'
  =
    gstate { infoToShow = ShowPause }
  | gstate == GameState {infoToShow = ShowGameState Player{ speed = s, health = h, position = (x,y)}
  = gstate { infoToShow = ShowGameState speler}
      where speler = d
inputKey _ gstate = gstate -- Otherwise keep the same