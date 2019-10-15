{
module Lexer where

import Token

}

%wrapper "basic"

$digit = 0-9		-- digits
$lowercase = [a-z]		-- alphabetic characters
$identchar = [A-Z]

tokens :-

  $white+				;
  "--".*				;
  "+"           { \s -> Plus }
  "-"           { \s -> Minus }
  "("					  { \s -> OpenParent }
  ")"           { \s -> CloseParent }
  "->"          { \s -> Arrow }
  "=="				  { \s -> Equal }
  "="				    { \s -> Assign }
  "Let"					{ \s -> Let }
  "In"					{ \s -> In }
  "Or"					{ \s -> Or }
  "And"					{ \s -> And }
  "Not"					{ \s -> Not }
  "Function"		{ \s -> Function }
  "If"					{ \s -> If }
  "Then"				{ \s -> Then }
  "Else"				{ \s -> Else }
  "True"				{ \s -> Bool True }
  "False"				{ \s -> Bool False }
  $digit+				{ \s -> Integer (read s) }
  $lowercase [$identchar $digit \_ \']*		{ \s -> Ident s }
