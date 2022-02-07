module Main where

import Prelude hiding (lookup)
import Expr
import Zipper
import FullZipper

import Control.Monad.State
import qualified Data.Map as M

main :: IO ()
main = putStrLn "expr zippers"

x -: f = f x

myExpr = Add (Num 1) (Mult (Num 2) (Num 3))

z :: FullZipper
z = mkZipper myExpr

z' :: FullZipper
z' = replace (z -: goLeft) (Num 4)

Just z'Top = goUp z'

cache = execState (evalZ z) []

cache' = execState (invalidateModified z') cache

cache'' = execState (evalZ z'Top) cache'

type Env a b = [(a, b)]

lookup :: (a -> Bool) -> [(a,b)] -> Maybe b
lookup p [] = Nothing
lookup p ((k,v):xs)
  | p k = Just v
  | otherwise = lookup p xs

insert :: a -> b -> [(a, b)] -> [(a, b)]
insert p q xs = xs ++ [(p,q)]

update :: (a -> a -> Bool) -> a -> b -> [(a, b)] -> [(a, b)]
update f k v [] = error "empty associative list"
update f k v ((k', v'):xs)
  | f k k' = (k',v):xs
  | otherwise = (k', v') : (update f k v xs)

delete :: (a -> a -> Bool) -> a -> [(a, b)] -> [(a, b)]
delete _ _ [] = error "empty associative list"
delete f k ((k', v'):xs)
  | f k k' = xs
  | otherwise = (k', v') : (delete f k xs)

evalZ :: (ExprZipper z) => z -> State (Env z Int) Int
evalZ z = do
  let e = unwrap z
  env <- get
  case lookup (`eqValue` z) env of
    Just x -> pure x
    Nothing -> case e of
      Num x -> modify (insert z x) >> pure x
      _     -> do
        left  <- evalZ $ goLeft z
        right <- evalZ $ goRight z
        let result = (op e) left right
        modify (insert z result) >> pure result

invalidateModified :: (ExprZipper z) => z -> State (Env z Int) ()
invalidateModified z = do
  modify (delete eqLocation z)
  case goUp z of
    Nothing -> pure ()
    Just z' -> invalidateModified z'

printList :: (Show a) => [a] -> IO ()
printList = mapM_ print
