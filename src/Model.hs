-- | This module contains the data types
--   which represent the state of the game
module Model where

import Player
import Bullet
import Enemy

data InfoToShow = ShowNothing
                | ShowGameState Player [Bullet]
                | ShowPause
                | ShowTest Player [Bullet] [Enemy]
                | ShowTriangle Player
                --         W of S _    Bullets Player _ Bullets enemies Score
                | ShowFinal Char Player [Bullet] [Enemy] [Bullet] Float

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 0.025
--WAS EERST 5, NU 2 OM TE TESTEN.

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0