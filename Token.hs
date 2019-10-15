module Token where

data FbToken
  = Plus
  | Minus
  | Or
  | And
  | Equal
  | Not
  | OpenParent
  | CloseParent
  | Function
  | Bool Bool
  | Integer Int
  | Ident String
  | Arrow
  | If
  | Then
  | Else
  | Let
  | In
  | Assign
  deriving (Eq, Ord, Show)
