module Main where

import Controller
import Model
import View
import Player
import Bullet
import Enemy
import Constanten                      

import Graphics.Gloss.Interface.IO.Game           
   
main :: IO ()
main = playIO (InWindow "Counter" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              20               -- Frames per second    
              initialState     -- Initial state
              view             -- View function  
              input            -- Event function   
              step             -- Step function                                                 