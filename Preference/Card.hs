module Preference.Card where

import Preference.Suite
import Preference.Utility

data Rank = Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace deriving (Eq, Ord, Enum)
    
instance Show Rank where
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "10"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

data Card = Card Rank Suite | Trump Rank Suite
    
class Suited a where
    inSuite :: a -> Card -> Bool

instance Suited Card where
    inSuite (Card  _ s1) (Card  _ s2) = s1 == s2
    inSuite (Trump _ s1) (Trump _ s2)
        | s1 == s2 = True
        | otherwise = error "Two trump suits"
    inSuite (Card _ s1) (Trump _ s2)
        | s1 /= s2 = True
        | otherwise = error "A suite is trump and not trump"
    inSuite x y = inSuite y x

isTrump :: Card -> Bool
isTrump (Trump _ _) = True
isTrump _ = False

beats :: Card -> Card -> Bool
beats x y
    | inSuite x y = (getRank x) > (getRank y)
    | otherwise = not $ isTrump y
    where
        getRank (Trump r1 _) = r1
        getRank (Card  r1 _) = r1

instance Eq Card where
    a == b = (inSuite a b) && (not $ beats a b) && (not $ beats b a)

eligible :: (Suited a) => a -> [Card] -> [Card]
eligible f = priority . (flist filters) where
    filters = [filter (inSuite f), filter isTrump, id]

deck :: [Card]
deck = cartesian (Card) (enumFrom Seven) (enumFrom Spades)

instance Show Card where
    show (Card r s) = show s ++ show r

card :: String -> Card
card (x:xs) = Card (parseR xs) (parseSuite x) where
    parseR "7" = Seven
    parseR "8" = Eight
    parseR "9" = Nine
    parseR "10" = Ten
    parseR "J" = Jack
    parseR "Q" = Queen
    parseR "K" = King
    parseR "A" = Ace

trump :: String -> Card
trump = tr . card where
    tr (Card x y) = Trump x y
