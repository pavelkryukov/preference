module Main where

import Card
import Player
import Suite
import Move

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

assertTrue :: Bool -> IO ()
assertTrue False = error "assertion failed!"
assertTrue _     = putStr "+"

assertFalse :: Bool -> IO ()
assertFalse = (assertTrue . not)

assertEq :: (Eq a) => a -> a -> IO ()
assertEq a b = assertTrue (a == b)

main = assertTrue  ((card "s7") `beats` (card "cA"))
    >> assertFalse ((card "s7") `beats` (card "sA"))
    >> assertTrue  ((card "sA") `beats` (card "dA"))
    >> assertFalse ((card "sA") `beats` (trump "dA"))
    >> assertEq North (trickWinner [Move North (card "s10"), Move East (card "dA"), Move South (card "cA")])
    >> assertEq South (trickWinner [Move North (card "s10"), Move East (card "dA"), Move South (card "sJ")])
    >> assertEq East  (trickWinner [Move North (card "s10"), Move East (trump "dA"), Move South (card "sJ")])