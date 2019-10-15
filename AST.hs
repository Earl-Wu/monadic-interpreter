-- This file contains the AST definition for the language

module AST
( Ident(..),
  Constraint(..),
  Expr(..),
  Fbtype(..),
  ) where

newtype Ident = Ident String deriving (Show, Eq, Ord)

data Constraint = Constraint Fbtype Fbtype deriving (Eq, Ord, Show)

data Expr = Var Ident | Function Ident Expr | Appl Expr Expr
  | Let Ident Expr Expr | LetRec Ident Ident Expr Expr
  | Plus Expr Expr | Minus Expr Expr | Equal Expr Expr
  | And Expr Expr | Or Expr Expr | Not Expr
  | If Expr Expr Expr | Int Int | Bool Bool deriving (Show)

data Fbtype = TInt | TBool | TArrow Fbtype Fbtype | TVar String deriving (Eq, Ord, Show)
