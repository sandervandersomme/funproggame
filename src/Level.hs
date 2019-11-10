module Level where

import Enemy

data Level = Level { idL :: LevelID, paren :: Paren}

type LevelID = Int

type Paren = [Paar]

type Paar  = (Int, Int, Enemy)

--Levels
level1 :: Level
level1 = Level 1 level1Paren

level1Paren :: Paren
level1Paren = [(1,2,getE 1), (2,4,getE 1), (3,3,getE 1), (4,1,getE 1), (5,4,getE 1), 
  (6,3,getE 2), (7,10,getE 1), (8,2,getE 3), (9,3,getE 3), (10,2,getE 2), 
  (11,3,getE 1), (12,2,getE 3), (13,5,getE 3), (14,12,getE 1), (15,4,getE 1), 
  (16,2,getE 1), (17,2,getE 1), (18,4,getE 3), (19,2,getE 1), (20,5,getE 2)]

level0 = Level 0 [(1,8,getE 1)]
