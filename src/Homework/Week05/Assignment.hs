module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit t) = t
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- #2
evalStr :: String -> Maybe Integer
-- evalStr s = fmap  eval (parseExp Lit Add Mul s)
-- after point free
evalStr = fmap eval . parseExp Lit Add Mul

-- #3
class Expr a where
  -- lit :: a -> a
  -- add :: a -> a -> a
  -- mul :: a -> a -> a
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- #4
instance Expr Integer where
  lit = id
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = if x > y then MinMax x else MinMax y
  mul (MinMax x) (MinMax y) = if x > y then MinMax y else MinMax x

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)
