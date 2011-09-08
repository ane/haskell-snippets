-- A simple binary tree
-- (c) ane <ane@iki.fi> 2009

module Main where

data Tree a = Root | Node a (Tree a) (Tree a) deriving (Eq, Show)

makeLeaf :: a -> Tree a
makeLeaf x = Node x Root Root

sizeTree :: Num a => Tree t -> a
sizeTree Root = 0
sizeTree (Node _ aTree bTree) = 1 + sizeTree aTree + sizeTree bTree

toList :: Tree a -> [a]
toList Root = []
toList (Node x aTree bTree) = x : toList aTree ++ toList bTree

fromList :: [a] -> Tree a
fromList [] = Root
fromList xs = foldl (flip insert) Root xs

insert :: Ord a => a -> Tree a -> Tree a
insert x Root = makeLeaf x
insert x (Node y aTree bTree) | x <= y    = Node y (insert x aTree) bTree
                              | otherwise = Node y aTree (insert x bTree)

main = putStrLn "hello"
