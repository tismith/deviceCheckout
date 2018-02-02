module Utils where

import Data.Maybe (maybe)

-- | Show a maybe, overriding the nothing case
-- >>> showMaybe Nothing
-- ""
-- >>> showMaybe (Just "test")
-- "\"test\""
showMaybe :: (Show s) => Maybe s -> String
showMaybe = maybe "" show

-- | Generate all the values for a particular type
-- >>> data TestType = A | B | C deriving (Bounded, Enum, Show)
-- >>> allValues :: [TestType]
-- [A,B,C]
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

-- | Eq over Maybe, with Nothing mapping to false
-- >>> maybeEq 1 (Just 1)
-- True
-- >>> maybeEq 1 Nothing
-- False
maybeEq :: (Eq a) => a -> Maybe a -> Bool
maybeEq value (Just value') = value == value'
maybeEq _ _  = False

