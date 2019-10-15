module Main where

import AST
import Interpreter
import TypeChecker
import Lexer
import Parser

main :: IO ()
main = do
  text <- getContents
  let tokenList = alexScanTokens text
  let ast = expr tokenList
  let typecorrect = typecheck ast
  case typecorrect of Right r -> print $ eval ast
                      Left str -> print $ str
  return ()
