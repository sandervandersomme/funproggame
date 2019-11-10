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
                | ShowFinal Char Player [Bullet] [Enemy] [Bullet] Float
                | ShowMenu 
                | ShowDead Player [Enemy] Float
                | ShowHighScores


data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0