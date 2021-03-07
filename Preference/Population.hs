module Preference.Population where

import Preference.Player

data Population = ThreePlayers | FourPlayers

nextPlayer :: Population -> (Player -> Player)
nextPlayer ThreePlayers = next where
    next West  = East
    next East  = South
    next South = West
nextPlayer FourPlayers = next where
    next West  = North
    next North = East
    next East  = South
    next South = West

dealer :: Population -> Player -> Player
dealer ThreePlayers = \_ -> Talon
dealer FourPlayers  = n . n . n
    where n = nextPlayer FourPlayers

players :: Population -> Player -> [Player]
players x f = [dealer x f] ++ (iterate (nextPlayer x) f)

