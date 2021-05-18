{-# language ScopedTypeVariables #-}
module Data.Debug.Diff where

-- containers
import Data.Tree (Tree(..))


-- | A type for labels for delta trees. If a tree has labels of type `a`, then
-- | we can represent a delta tree with labels of type `Delta a`.
data Delta a
  -- Each occurrence of this label corresponds to a node where the two trees
  -- being compared are identical.
  = Same a

  -- This label indicates that the two trees being compared differ at the node
  -- being labelled. Every node with this label should have exactly two
  -- children: the first being the subtree rooted here in the first of the two
  -- trees being diffed, and the second being the subtree rooted here in the
  -- second tree.
  | Different

  -- This label indicates that the first of the trees being diffed has a
  -- subtree here, whereas the second does not. It should have exactly one
  -- child: the subtree of the first tree rooted at the point where it appears.
  | Extra1

  -- This label indicates that the second of the trees being diffed has a
  -- subtree here, whereas the second does not. It should have exactly one
  -- child: the subtree of the second tree rooted at the point where it
  -- appears.
  | Extra2

  -- This label indicates that we are in a differing subtree (and hence are not
  -- going to bother to perform any more diffing).
  | SubTree a




diffWith
  :: forall a
  .  (a -> a -> Bool)
  -> (a -> Bool)
  -> Tree a
  -> Tree a
  -> Tree (Delta a)
diffWith fuzzyEq isUnimportant = go
  where
    go :: Tree a -> Tree a -> Tree (Delta a)
    go left@(Node x xs) right@(Node y ys) =
      if not (fuzzyEq x y) then
        Node Different [fmap SubTree left, fmap SubTree right]
      else
        let
          diffChildren = goChildren xs ys
        in
          if isUnimportant x && all hasDiff diffChildren then
            Node Different [fmap SubTree left, fmap SubTree right]
          else
            Node (Same x) diffChildren

    goChildren :: [Tree a] -> [Tree a] -> [Tree (Delta a)]
    goChildren left right =
      case compare leftLen rightLen of
        LT -> begin <> map (extra Extra1) (drop leftLen right)
        EQ -> begin
        GT -> begin <> map (extra Extra2) (drop rightLen left)

      where
        begin    = zipWith go left right
        leftLen  = length left
        rightLen = length right

hasDiff :: Tree (Delta a) -> Bool
hasDiff (Node root _) =
  case root of
    Same _ -> False
    _      -> True

extra :: Delta a -> Tree a -> Tree (Delta a)
extra cTor subTree = Node cTor [fmap SubTree subTree]
