module Main where

import Controller
import Model
import View
import Player
import Bullet

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (1200, 800) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function                             