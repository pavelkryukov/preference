module Preference.Hand where

import Preference.Card
import Preference.Move
import Preference.Player
import Preference.Suite

data Hand = Hand Player [Card]

instance Show Hand where
    show (Hand p c) = show p ++ "|" ++ show c

eligibleMoves :: [Move] -> Hand -> [Move]
eligibleMoves m (Hand p c) = map (Move p) list where
    list
        | null m    = c
        | otherwise = eligible (head m) c
