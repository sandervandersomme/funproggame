module Player where

data Player = Player { speed :: Speed, health :: Health, position :: Position }

type Position = (Float, Float)

type Health = Int

type Speed = Int

