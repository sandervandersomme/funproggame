module HulpFuncties where

import Model
import Level

fst2 :: (a, b, c) -> a
fst2 (x,_,_) = x

snd2 :: (a, b, c) -> b
snd2 (_,x,_) = x

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

scoreString :: [String] -> [Int]
scoreString stringss = [(read x) | x <- stringss]

updateHighScores :: Int -> [Int] -> [Int]
updateHighScores score [] = []
updateHighScores score lijst@(sc:scs) 
  | score >= sc = [score] ++ (take (4-(5 - (length lijst))) (sc:scs))
  | otherwise = [sc] ++ (updateHighScores score scs)

scoresToString :: [Int] -> String
scoresToString [] = ""
scoresToString (sc:scs) = (show sc) ++ "\n" ++ (scoresToString scs)

getHighScoreText :: GameState -> String
getHighScoreText gs@GameState{infoToShow=ShowHighScores _ s} = s
getHighScoreText gs = ""

updateScore :: Float -> Float -> Float
updateScore score points
  | score + points <= 0 = 0
  | otherwise = score + points

gstateIsShowLevel :: GameState -> Bool
gstateIsShowLevel gs@GameState{infoToShow = ShowLevel _ _ _ _ _ _ _ _} = True
gstateIsShowLevel gs = False

gstateIsShowhighscores :: GameState -> Bool
gstateIsShowhighscores gs@GameState{infoToShow = ShowHighScores _ _} = True
gstateIsShowhighscores gs = False

gstateIsShowdead :: GameState -> Bool
gstateIsShowdead gs@GameState{infoToShow = ShowHighScores _ _ } = True
gstateIsShowdead gs = False

getScore :: GameState -> IO Float
getScore GameState{infoToShow=ShowDead _ _ c _} = return c
getScore gs = return (-1)

aantalEnemies :: GameState -> IO Int
aantalEnemies GameState{infoToShow=ShowLevel _ _ _ _ _ _ _ Level{paren=((_,a,_):ps)}} = return a
aantalEnemies gs = return 0