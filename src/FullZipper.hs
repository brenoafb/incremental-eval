{-# LANGUAGE FlexibleInstances #-}
module FullZipper where

import Expr
import Zipper

data OpC = AddC | MultC
  deriving (Eq, Show)

data ExprC = LeftC  OpC Expr
           | RightC OpC Expr
           deriving (Eq, Show)

data FullZipper = FullZipper { curr :: Expr, context :: [ExprC] }
  deriving (Eq, Show)

instance ExprZipper FullZipper where
  mkZipper e = FullZipper e []

  unwrap (FullZipper e _) = e

  replace (FullZipper _ c) e = FullZipper e c

  goUp (FullZipper e cs) = case cs of
    [] -> Nothing
    (LeftC AddC e' : cs')   -> Just (FullZipper (Add e e') cs')
    (RightC AddC e' : cs')  -> Just (FullZipper (Add e' e) cs')
    (LeftC MultC e' : cs')  -> Just (FullZipper (Mult e e') cs')
    (RightC MultC e' : cs') -> Just (FullZipper (Mult e' e) cs')

  goLeft (FullZipper e cs) = case e of
    Add el er  -> FullZipper el (LeftC AddC er : cs)
    Mult el er -> FullZipper el (LeftC MultC er : cs)

  goRight (FullZipper e cs) = case e of
    Add el er  -> FullZipper er (RightC AddC el : cs)
    Mult el er -> FullZipper er (RightC MultC el : cs)

  eqLocation (FullZipper _ cs) (FullZipper _ ds) = go cs ds
    where go [] [] = True
          go [] _  = False
          go _  [] = False
          go (c:cs) (d:ds) =
            case (c, d) of
              (LeftC _ _, LeftC _ _) -> go cs ds
              (RightC _ _, RightC _ _) -> go cs ds
              (_, _) -> False

  eqValue (FullZipper e1 _) (FullZipper e2 _) = e1 == e2
