module Preference.Move where

import Preference.Card
import Preference.Player
import Preference.Suite

data Move = Move Player Card

instance Show Move where
    show (Move p c) = show p ++ "|" ++ show c

trickWinner :: [Move] -> Player
trickWinner (x:xs) = pl $ foldl better x xs
    where
        pl (Move p c) = p
        better x y
            | x `bs` y  = x
            | otherwise = y
        (Move p1 c1) `bs` (Move p2 c2)
            | p1 == p2  = error "Two player moves"
            | otherwise = c1 `beats` c2

instance Suited Move where
    inSuite (Move _ x) y = inSuite x y
