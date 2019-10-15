{
module Parser where

import AST
import Lexer
import qualified Token as T

}

%name expr
%tokentype { T.FbToken }

%error{ parseError }

%token
      "+"           { T.Plus }
      "-"           { T.Minus }
      "("					  { T.OpenParent }
      ")"           { T.CloseParent }
      "->"          { T.Arrow }
      "="				    { T.Assign }
      "=="				  { T.Equal }
      "Let"					{ T.Let }
      "In"					{ T.In }
      "Or"					{ T.Or }
      "And"					{ T.And }
      "Not"					{ T.Not }
      "Function"		{ T.Function }
      "If"					{ T.If }
      "Then"				{ T.Then }
      "Else"				{ T.Else }
      "True"				{ T.Bool true }
      "False"				{ T.Bool false }
      int				    { T.Integer $$ }
      ident	        { T.Ident $$ }

%right FUN IF LET
%left "Or"
%left "And"
%left "Not"
%left "=="
%left "+" "-"

%%

Expr : Appl_expr        { $1 }
     | Expr "+" Expr   { Plus $1 $3 }
     | Expr "-" Expr  { Minus $1 $3 }
     | Expr "And" Expr    { And $1 $3 }
     | Expr "Or" Expr     { Or $1 $3 }
     | "Not" Expr         { Not $2 }
     | Expr "==" Expr  { Equal $1 $3 }
     | "Function" Ident "->" Expr %prec FUN { Function $2 $4 }
     | "Let" Ident "=" Expr "In" Expr %prec LET   { Let $2 $4 $6 }
     | "If" Expr "Then" Expr "Else" Expr %prec IF { If $2 $4 $6 }

Appl_expr : Simple_expr { $1 }
          | Appl_expr Simple_expr { Appl $1 $2 }

Simple_expr : int { Int $1 }
            | "True" { Bool True }
            | "False" { Bool False }
            | Ident { Var $1 }
            | "(" Expr ")" { $2 }

Ident : ident { Ident $1 }

{
parseError :: [T.FbToken] -> a
parseError _ = error "Parse error"
}
