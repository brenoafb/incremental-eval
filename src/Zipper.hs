module Zipper where

import Expr

class ExprZipper z where
  mkZipper :: Expr -> z
  unwrap :: z -> Expr
  replace :: z -> Expr -> z
  goUp :: z -> Maybe z
  goLeft :: z -> z
  goRight :: z -> z
  eqLocation :: z -> z -> Bool
  eqValue :: z -> z -> Bool
  eqLocationValue :: z -> z -> Bool
  eqLocationValue z1 z2 =
    z1 `eqLocation` z2 && z1 `eqValue` z2
