-- | This module contains the data types
--   which represent the state of the game
module Model where

import Player
import Bullet

data InfoToShow = ShowNothing
                | ShowGameState Player [Bullet]
                | ShowPause
                | ShowBullets [Bullet]

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 2
--WAS EERST 5, NU 2 OM TE TESTEN.

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0