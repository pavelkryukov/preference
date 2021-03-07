module Main where

import Preference.Assert
import Preference.Card
import Preference.Player
import Preference.Suite
import Preference.Move

data TricksAmount = SixTricks
    | SevenTricks
    | EightTricks
    | NineTricks
    | TenTricks deriving (Eq, Ord)

data Game = Pass
    | Misere
    | Tricks TricksAmount (Maybe Suite) deriving (Eq)

instance Ord Game where
    Pass              <= _                = True
    Misere            <= Tricks a _       = NineTricks <= a
    Tricks _ (Just x) <= Tricks _ Nothing = True
    Tricks a (Just x) <= Tricks b (Just y)
        | a  < b = True
        | a == b = x <= y
        | a  > b = False

s7  = card "♠7"
cA  = card "♣A"
sA  = card "♠A"
s10 = card "♠10"
sJ  = card "♠J"
dA  = card "♦A"
dA' = trump "♦A"

cards  = [s7, s10, sJ, sA, cA, dA]
cards' = [s7, s10, sJ, sA, cA, dA']

main = assertTrue  (s7 `beats` cA)
    >> assertFalse (s7 `beats` sA)
    >> assertTrue  (sA `beats` dA)
    >> assertFalse (sA `beats` dA')
    >> assertEq North (trickWinner [Move North s10, Move East dA,  Move South cA])
    >> assertEq South (trickWinner [Move North s10, Move East dA,  Move South sJ])
    >> assertEq East  (trickWinner [Move North s10, Move East dA', Move South sJ])
    >> assertEq [s7, s10, sJ, sA] (eligible (card "sQ") cards)
    >> assertEq cards (eligible (card "hQ") cards)
    >> assertEq [dA'] (eligible (card "hQ") cards')
    >> assertEq [cA] (eligible (card "cQ") cards)
