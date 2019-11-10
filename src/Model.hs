-- | This module contains the data types
--   which represent the state of the game
module Model where

import Player
import Bullet
import Enemy
import Level
import Constanten

data InfoToShow = ShowNothing
                | ShowPause InfoToShow Float
                | ShowFinal Char Player [Bullet] [Enemy] Float
                | ShowMenu Char Int
                | Show1v1 Char Player Char Player [Bullet] [Bullet] [Enemy] [Enemy]
                | ShowLevel Char Player [Bullet] [Enemy] [Bullet] Float Int Level
                | ShowHighScores [String] String
                | ShowDead Player [Enemy] Float String


data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState (ShowMenu '_' 1)0