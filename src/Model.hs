-- | This module contains the data types
--   which represent the state of the game
module Model where

import Player

data InfoToShow = ShowNothing
                | ShowGameState Player
                | ShowPause

nOSECSBETWEENCYCLES :: Float
nOSECSBETWEENCYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0