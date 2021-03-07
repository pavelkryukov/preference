module Hand where

import Card
import Move
import Player
import Suite

data Hand = Hand Player [Card]

instance Show Hand where
    show (Hand p c) = show p ++ "|" ++ show c

eligibleMoves :: [Move] -> Hand -> [Move]
eligibleMoves m (Hand p c) = map (Move p) list where
    list
        | null m    = c
        | otherwise = eligible (head m) c
