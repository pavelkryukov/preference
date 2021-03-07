module Preference.Assert where

assertTrue :: Bool -> IO ()
assertTrue False = error "assertion failed!"
assertTrue _     = putStr "+"

assertFalse :: Bool -> IO ()
assertFalse = (assertTrue . not)

assertEq :: (Eq a) => a -> a -> IO ()
assertEq a b = assertTrue (a == b)
