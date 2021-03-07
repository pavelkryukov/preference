module Player where

data Player = West | East | South | North | Talon deriving (Eq)
instance Show Player where
    show West  = "West"
    show East  = "East"
    show South = "South"
    show North = "North"
    show Talon = "Talon"
