module Proof.Matcher.Core where

import Data.List

type FailMessage = String
type Evaluator actual expected = actual -> expected -> Bool
type Matcher actual expected = (FailMessage, Evaluator actual expected)

toBe :: Eq a => Matcher a a
toBe = ("Expected <@actual@> to be <@expected@>", (==))

toBeF :: Matcher Float Float
toBeF = ("Expected <@actual@> to be <@expected@>", (\x y -> (abs (x - y)) < 0.01))

toNotBe :: Eq a => Matcher a a
toNotBe = ("Expected <@actual@> to not be <@expected@>", (/=))


toBeSorted :: Ord a => Matcher [a] [a]
toBeSorted = ("Expected sorted <@actual@> to be sorted <@expected@>", 
   \left right -> ((sortBy compare left) == (sortBy compare right)))
