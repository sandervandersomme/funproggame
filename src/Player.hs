module Player where

data Player = Player { speed :: Speed, health :: Health, position :: Position, 
  bullCou :: BulletCount, immuCou :: ImmunityCount }

type Position = (Float, Float)

type Health = Int

type Speed = Float

type BulletCount = Int

type ImmunityCount = Int