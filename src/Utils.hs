module Utils where

import Data.Maybe (maybe)

showMaybe :: (Show s) => Maybe s -> String
showMaybe = maybe "" show

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

maybeEq :: (Eq a) => a -> Maybe a -> Bool
maybeEq value (Just value') = value == value'
maybeEq _ _  = False

