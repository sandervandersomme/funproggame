-- | This module contains the data types
--   which represent the state of the game
module Model where

import Player
import Bullet
import Enemy

data InfoToShow = ShowNothing
                | ShowGameState Player [Bullet]
                | ShowPause
                | ShowBullets [Bullet]
                | ShowTest Player [Bullet] [Enemy]
                | ShowTriangle Player
                --         W of S _    Bullets Player _ Bullets enemies Score
                | ShowFinal Char Player [Bullet] [Enemy] [Bullet] Int

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 0.25
--WAS EERST 5, NU 2 OM TE TESTEN.

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0