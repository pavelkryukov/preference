module Preference.Assert where

import Data.List (isPrefixOf)

assertTrue :: Bool -> IO ()
assertTrue False = error "assertion failed!"
assertTrue _     = putStr "+"

assertFalse :: Bool -> IO ()
assertFalse = (assertTrue . not)

assertEq :: (Eq a) => a -> a -> IO ()
assertEq a b = assertTrue $ a == b

assertIn :: (Eq a) => [a] -> [a] -> IO ()
assertIn a b = assertTrue $ isPrefixOf a b
