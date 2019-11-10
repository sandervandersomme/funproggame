module Player where

import Constanten

data Player = Player { speed :: Speed, health :: Health, position :: Position, 
  bullCou :: BulletCount, immuCou :: ImmunityCount }

type Position = (Float, Float)

type Health = Int

type Speed = Float

type BulletCount = Int

type ImmunityCount = Int

player1 :: Player
player1 = Player 5 3 (spX, spY) 0 0 

player2 :: Player
player2 = Player 5 3 (-spX, spY) 0 0 