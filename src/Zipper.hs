module Zipper
    ( Zipper
    , fromList
    , toList
    , reset
    , cursor
    , left
    , right
    , insert
    ) where

import           Data.Maybe


data Zipper a = Zipper [a] [a]
    deriving (Show, Eq)


fromList :: [a] -> Zipper a
fromList = Zipper []

toList :: Zipper a -> [a]
toList (Zipper ls rs) = reverse ls ++ rs

reset :: Zipper a -> Zipper a
reset = fromList . toList

cursor :: Zipper a -> Maybe a
cursor (Zipper _ rs) = listToMaybe rs

left :: Zipper a -> Maybe (Zipper a)
left (Zipper [] _) = Nothing
left (Zipper (a:ls) rs) = Just $ Zipper ls (a:rs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ []) = Nothing
right (Zipper ls (a:rs)) = Just $ Zipper (a:ls) rs

insert :: a -> Zipper a -> Zipper a
insert a (Zipper ls rs) = Zipper ls (a:rs)
