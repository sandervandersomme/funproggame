-- | This module contains the data types
--   which represent the state of the game
module Model where

import Player
import Bullet

data InfoToShow = ShowNothing
                | ShowGameState Player [Bullet]
                | ShowPause
                | ShowBullets [Bullet]
                | ShowTest Player [Bullet]
                | ShowTriangle Player

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 0.25
--WAS EERST 5, NU 2 OM TE TESTEN.

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0