module Suite where

data Suite = Spades
    | Clubs
    | Diamonds
    | Hearts deriving (Eq, Ord)

instance Show Suite where
    show Spades   = "♠"
    show Clubs    = "♣"
    show Diamonds = "♦"
    show Hearts   = "♥"
    
parseSuite :: Char -> Suite
parseSuite '♠' = Spades
parseSuite '♣' = Clubs
parseSuite '♦' = Diamonds
parseSuite '♥' = Hearts
parseSuite 's' = Spades
parseSuite 'c' = Clubs
parseSuite 'd' = Diamonds
parseSuite 'h' = Hearts
