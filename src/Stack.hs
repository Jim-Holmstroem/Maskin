module Stack
    ( Stack
    , isEmpty
    , push
    , pop
    , empty
    ) where

import           Data.Maybe

data Stack a = Stack [a]
    deriving (Show, Eq)


empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

pop :: Stack a -> Maybe (Stack a, a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (Stack xs, x)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:xs)) = Just x
