{-# LANGUAGE TypeSynonymInstances #-}
module PathZipper where

import Expr
import Zipper

data ExprPath = L | R
  deriving (Eq, Show)

data PathZipper = PathZipper { curr :: Expr, root :: Expr, path :: [ExprPath] }
  deriving Show

getPathZipper :: Expr -> [ExprPath] -> Expr
getPathZipper e [] = e
getPathZipper (Add e _) (L:ps) = getPathZipper e ps
getPathZipper (Mult e _) (L:ps) = getPathZipper e ps
getPathZipper (Add _ e) (R:ps) = getPathZipper e ps
getPathZipper (Mult _ e) (R:ps) = getPathZipper e ps

instance ExprZipper PathZipper where
  mkZipper e = PathZipper { curr = e, root = e, path = [] }

  unwrap = curr

  -- replace pz e = pz { curr = e } -- TODO replace child in the root as well
  replace pz e = undefined

  goUp (PathZipper _ _ []) = Nothing
  goUp (PathZipper _ root (p:ps)) =
    Just $ PathZipper curr root ps
    where curr = getPathZipper root ps

  goLeft (PathZipper (Add e _) root ps) =
    PathZipper e root (L:ps)

  goLeft (PathZipper (Mult e _) root ps) =
    PathZipper e root (L:ps)

  goRight (PathZipper (Add _ e) root ps) =
    PathZipper e root (R:ps)

  goRight (PathZipper (Mult _ e) root ps) =
    PathZipper e root (R:ps)

  eqLocation (PathZipper _ _ p1) (PathZipper _ _ p2) = p1 == p2

  eqValue (PathZipper c1 _ _) (PathZipper c2 _ _) = c1 == c2
