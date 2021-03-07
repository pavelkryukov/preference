module Preference.Utility where

-- | If the first list is not empty, returns it;
--   otherwise tries recurslively with tail
priority :: [[a]] -> [a]
priority (x:xs)
    | null x    = priority xs
    | otherwise = x

-- | Applies a list of functions to a given object
flist :: [a -> b] -> a -> [b]
flist fs a = map ($ a) fs

-- | Cartesian product
cartesian :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesian f (x:xs) y = (map (f x) y) ++ cartesian f xs y
cartesian f [] _ = []
